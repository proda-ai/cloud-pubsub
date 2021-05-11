module Cloud.PubSub
  ( AuthMethod(..)
  , GoogleApiConfig(..)
  , mkClientResources
  , mkPubSubEnv
  ) where


import qualified Cloud.PubSub.Auth             as Auth
import           Cloud.PubSub.Http.Types        ( ClientResources(..)
                                                , TokenContainer(NotInitialized)
                                                )
import           Cloud.PubSub.IO                ( PubSubEnv(..) )
import qualified Cloud.PubSub.Logger           as Logger
import qualified Control.Concurrent.MVar       as MVar
import           Data.Time                      ( NominalDiffTime )
import qualified Network.HTTP.Client           as HttpClient
import qualified Network.HTTP.Client.TLS       as HttpClientTLS

newtype AuthMethod = ServiceAccountFile FilePath
                  deriving (Show, Eq)

data GoogleApiConfig = GoogleApiConfig
  { authMethod          :: AuthMethod
  , tokenRenewThreshold :: NominalDiffTime
  }
  deriving (Show, Eq)

mkClientResources :: GoogleApiConfig -> IO ClientResources
mkClientResources config = case authMethod config of
  ServiceAccountFile file ->
    ClientResources
      <$> Auth.readServiceAccountFile file
      <*> MVar.newMVar NotInitialized
      <*> HttpClient.newManager HttpClientTLS.tlsManagerSettings -- Manager closed automatically
      <*> pure (tokenRenewThreshold config)

mkPubSubEnv :: GoogleApiConfig -> Logger.Logger -> IO PubSubEnv
mkPubSubEnv c l = PubSubEnv l <$> mkClientResources c
