module Cloud.PubSub
  ( AuthMethod(..)
  , CloudConfig(..)
  , HostAndPort(..)
  , PubSubTarget(..)
  , mkClientResources
  , mkPubSubEnv
  ) where


import qualified Cloud.PubSub.Auth             as Auth
import           Cloud.PubSub.Core.Types        ( ProjectId )
import           Cloud.PubSub.Http.Types        ( ClientResources(..)
                                                , CloudTargetResources(..)
                                                , TargetResources(..)
                                                , TokenContainer(NotInitialized)
                                                )
import           Cloud.PubSub.Trans             ( PubSubEnv(..) )
import qualified Control.Concurrent.MVar       as MVar
import           Data.Time                      ( NominalDiffTime )
import qualified Network.HTTP.Client           as HttpClient
import qualified Network.HTTP.Client.TLS       as HttpClientTLS

newtype AuthMethod = ServiceAccountFile FilePath
                  deriving (Show, Eq)

newtype HostAndPort = HostAndPort { unwrapHostAndPort :: String }
                       deriving (Show, Eq)

data CloudConfig = CloudConfig
  { tokenRenewThreshold :: NominalDiffTime
  , authMethod          :: AuthMethod
  }
  deriving (Show, Eq)

data PubSubTarget = EmulatorTarget HostAndPort
                  | CloudServiceTarget CloudConfig
                    deriving (Show,Eq)

mkClientResources :: ProjectId -> PubSubTarget -> IO ClientResources
mkClientResources projectId target = do
  (url, targetResources) <- case target of
    EmulatorTarget hostAndPort -> do
      let hostAndPortUrl = "http://" <> unwrapHostAndPort hostAndPort
      return (hostAndPortUrl, Emulator)
    CloudServiceTarget (CloudConfig threshold (ServiceAccountFile saFile)) ->
      do
        resources <-
          CloudTargetResources
          <$> Auth.readServiceAccountFile saFile
          <*> MVar.newMVar NotInitialized
          <*> pure threshold
        let serviceUrl = "https://pubsub.googleapis.com"
        return (serviceUrl, Cloud resources)
  -- Manager closed automatically
  manager <- HttpClient.newManager HttpClientTLS.tlsManagerSettings
  return $ ClientResources manager url projectId targetResources

mkPubSubEnv :: ProjectId -> PubSubTarget -> IO PubSubEnv
mkPubSubEnv pId psTarget = PubSubEnv <$> mkClientResources pId psTarget
