module Cloud.PubSub.Auth.Token where

import qualified Cloud.PubSub.Auth             as Auth
import qualified Cloud.PubSub.Auth.Types       as Auth
import qualified Cloud.PubSub.Http.Types       as HttpT
import qualified Cloud.PubSub.Logger           as Logger
import qualified Control.Concurrent            as MVar
import           Control.Monad.Catch            ( onException )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.Time                     as Time

fetchAndUpdateToken
  :: HttpT.ClientResources -> Auth.Scope -> IO Auth.AccessToken
fetchAndUpdateToken resources scope = do
  let serviceAccount = HttpT.crServiceAccount resources
      manager        = HttpT.crManager resources
  tokenResponse <- Auth.fetchToken manager serviceAccount scope
  now           <- Time.getCurrentTime
  let accessToken =
        Auth.accessToken (tokenResponse :: Auth.AccessTokenResponse)
      expiresIn   = Auth.expiresIn (tokenResponse :: Auth.AccessTokenResponse)
      expiresAt   = Time.addUTCTime expiresIn now
      cachedToken = Auth.CachedToken accessToken expiresAt
  MVar.putMVar (HttpT.crCachedTokenMVar resources) (HttpT.Available cachedToken)
  return accessToken

fetchAndUpdateTokenOrReset
  :: MonadIO m => HttpT.ClientResources -> Auth.Scope -> m Auth.AccessToken
fetchAndUpdateTokenOrReset resources scope =
  liftIO
    $             fetchAndUpdateToken resources scope
    `onException` MVar.tryPutMVar (HttpT.crCachedTokenMVar resources)
                                  HttpT.NotInitialized

getToken
  :: (HttpT.HasClientResources m, Logger.HasLogger m, MonadIO m)
  => Auth.Scope
  -> m Auth.AccessToken
getToken scope = do
  resources <- HttpT.askClientResources
  let tokenMVar = HttpT.crCachedTokenMVar resources
  liftIO (MVar.takeMVar tokenMVar) >>= \case
    HttpT.NotInitialized -> do
      Logger.log Logger.Debug Nothing "Fetching initial token"
      fetchAndUpdateTokenOrReset resources scope
    HttpT.Available cachedToken -> do
      now <- liftIO Time.getCurrentTime
      let renewThreshold = HttpT.crRenewThreshold resources
      if Time.diffUTCTime (Auth.expiresAt cachedToken) now < renewThreshold
        then do
          Logger.log Logger.Debug Nothing "Token expired, fetching token"
          fetchAndUpdateTokenOrReset resources scope
        else do
          liftIO $ MVar.putMVar tokenMVar (HttpT.Available cachedToken)
          Logger.log Logger.Debug Nothing "Using cached token"
          return $ Auth.accessToken (cachedToken :: Auth.CachedToken)
