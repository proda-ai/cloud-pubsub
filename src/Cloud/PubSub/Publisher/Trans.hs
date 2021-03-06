module Cloud.PubSub.Publisher.Trans
  ( PublisherEnv(..)
  , PublisherT
  , runPublisherT
  ) where

import qualified Cloud.PubSub.Auth.Token       as AuthToken
import qualified Cloud.PubSub.Auth.Types       as AuthT
import qualified Cloud.PubSub.Http.Types       as HttpT
import qualified Cloud.PubSub.Logger           as Logger
import qualified Cloud.PubSub.Publisher.Types  as PublisherT
import           Control.Monad.Catch            ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                )
import           Control.Monad.IO.Class         ( MonadIO )
import qualified Control.Monad.Logger          as ML
import qualified Control.Monad.Reader          as Reader
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                )

data PublisherEnv = PublisherEnv
  { clientResources    :: HttpT.ClientResources
  , publisherResources :: PublisherT.PublisherResources
  }

newtype PublisherT m a = PublisherT
  { runPublisherT_ :: ReaderT PublisherEnv (ML.LoggingT m) a }
  deriving newtype(
    Functor, Applicative, Monad, MonadFail,
    MonadIO, MonadThrow, MonadCatch, MonadMask,
    MonadReader PublisherEnv, ML.MonadLogger)

instance Monad m => HttpT.HasClientResources (PublisherT m) where
  askClientResources = Reader.asks clientResources
instance Monad m => HttpT.HasGoogleProjectId (PublisherT m)
instance Monad m => HttpT.HasPubSubHttpManager (PublisherT m)

instance MonadIO m => AuthT.GoogleApiAuth (PublisherT m) where
  getToken = AuthToken.getToken

instance Monad m => PublisherT.HasPublisherResources (PublisherT m) where
  askPublisherResources = Reader.asks publisherResources

runPublisherT :: Logger.LoggerFn -> PublisherEnv -> PublisherT m a -> m a
runPublisherT logger resources action = do
  ML.runLoggingT (Reader.runReaderT (runPublisherT_ action) resources)
                 logger
