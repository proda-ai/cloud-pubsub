module Cloud.PubSub.Trans
  ( PubSubEnv(..)
  , PubSubT
  , runPubSubT
  ) where

import qualified Cloud.PubSub.Auth.Token       as AuthToken
import qualified Cloud.PubSub.Auth.Types       as AuthT
import qualified Cloud.PubSub.Http.Types       as HttpT
import qualified Cloud.PubSub.Logger           as Logger
import           Control.Monad.Catch            ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                )
import           Control.Monad.Reader           ( MonadIO
                                                , MonadReader
                                                , ReaderT
                                                )
import qualified Control.Monad.Reader          as Reader

data PubSubEnv = PubSubEnv
  { logger          :: Logger.Logger
  , clientResources :: HttpT.ClientResources
  }

newtype PubSubT m a = PubSubT
  { runPubSubT_ :: ReaderT PubSubEnv m a }
  deriving newtype(
    Functor, Applicative, Monad, MonadFail,
    MonadIO, MonadThrow, MonadCatch, MonadMask,
    MonadReader PubSubEnv)

instance Monad m => HttpT.HasClientResources (PubSubT m) where
  askClientResources = Reader.asks clientResources
instance Monad m => HttpT.HasGoogleProjectId (PubSubT m)
instance Monad m => HttpT.HasPubSubHttpManager (PubSubT m)

instance MonadIO m => AuthT.GoogleApiAuth (PubSubT m) where
  getToken = AuthToken.getToken

instance Monad m => Logger.HasLogger (PubSubT m) where
  askLogger = Reader.asks logger

runPubSubT :: PubSubEnv -> PubSubT m a -> m a
runPubSubT resources action = Reader.runReaderT (runPubSubT_ action) resources
