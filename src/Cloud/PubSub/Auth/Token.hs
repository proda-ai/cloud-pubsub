module Cloud.PubSub.Auth.Token
  ( getToken
  ) where

import qualified Cloud.PubSub.Auth             as Auth
import qualified Cloud.PubSub.Auth.Types       as Auth
import qualified Cloud.PubSub.Http.Types       as HttpT
import qualified Cloud.PubSub.Logger           as Logger
import qualified Control.Concurrent            as MVar
import           Control.Monad.Catch            ( onException )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Logger           ( MonadLogger )
import qualified Data.Time                     as Time
import qualified Network.HTTP.Client           as HttpClient

fetchAndUpdateToken
  :: HttpT.CloudTargetResources
  -> HttpClient.Manager
  -> Auth.Scope
  -> IO Auth.AccessToken
fetchAndUpdateToken resources manager scope = do
  let serviceAccount = HttpT.ctrServiceAccount resources
  tokenResponse <- Auth.fetchToken manager serviceAccount scope
  now           <- Time.getCurrentTime
  let accessToken =
        Auth.accessToken (tokenResponse :: Auth.AccessTokenResponse)
      expiresIn   = Auth.expiresIn (tokenResponse :: Auth.AccessTokenResponse)
      expiresAt   = Time.addUTCTime expiresIn now
      cachedToken = Auth.CachedToken accessToken expiresAt
  MVar.putMVar (HttpT.ctrCachedTokenMVar resources)
               (HttpT.Available cachedToken)
  return accessToken

fetchAndUpdateTokenOrReset
  :: MonadIO m
  => HttpT.CloudTargetResources
  -> HttpClient.Manager
  -> Auth.Scope
  -> m Auth.AccessToken
fetchAndUpdateTokenOrReset resources manager scope =
  liftIO
    $             fetchAndUpdateToken resources manager scope
    `onException` MVar.tryPutMVar (HttpT.ctrCachedTokenMVar resources)
                                  HttpT.NotInitialized

getToken
  :: (HttpT.HasClientResources m, MonadLogger m, MonadIO m)
  => Auth.Scope
  -> m (Maybe Auth.AccessToken)
getToken scope = do
  clientResources <- HttpT.askClientResources
  case HttpT.crTargetResorces clientResources of
    HttpT.Emulator             -> return Nothing
    HttpT.Cloud cloudResources -> Just <$> do
      let manager   = HttpT.crManager clientResources
          tokenMVar = HttpT.ctrCachedTokenMVar cloudResources
      liftIO (MVar.takeMVar tokenMVar) >>= \case
        HttpT.NotInitialized -> do
          Logger.logWithContext Logger.Debug Nothing "Fetching initial token"
          fetchAndUpdateTokenOrReset cloudResources manager scope
        HttpT.Available cachedToken -> do
          now <- liftIO Time.getCurrentTime
          let renewThreshold = HttpT.ctrRenewThreshold cloudResources
          if Time.diffUTCTime (Auth.expiresAt cachedToken) now < renewThreshold
            then do
              Logger.logWithContext Logger.Debug
                                    Nothing
                                    "Token expired, fetching token"
              fetchAndUpdateTokenOrReset cloudResources manager scope
            else do
              liftIO $ MVar.putMVar tokenMVar (HttpT.Available cachedToken)
              Logger.logWithContext Logger.Debug Nothing "Using cached token"
              return $ Auth.accessToken (cachedToken :: Auth.CachedToken)
