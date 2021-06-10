module Cloud.PubSub.Publisher.IO
  ( PublisherEnv(..)
  , PublisherIO(..)
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
import qualified Control.Monad.Reader          as Reader
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                )

data PublisherEnv = PublisherEnv
  { logger             :: Logger.Logger
  , clientResources    :: HttpT.ClientResources
  , publisherResources :: PublisherT.PublisherResources
  }

newtype PublisherIO a = PublisherIO
  { runPublisherIO :: ReaderT PublisherEnv IO a }
  deriving newtype(
    Functor, Applicative, Monad, MonadFail,
    MonadIO, MonadThrow, MonadCatch, MonadMask,
    MonadReader PublisherEnv)

instance HttpT.HasClientResources PublisherIO where
  askClientResources = Reader.asks clientResources
instance HttpT.HasGoogleProjectId PublisherIO
instance HttpT.HasPubSubHttpManager PublisherIO

instance AuthT.GoogleApiAuth PublisherIO where
  getToken = AuthToken.getToken

instance Logger.HasLogger PublisherIO where
  askLogger = Reader.asks logger

instance PublisherT.HasPublisherResources PublisherIO where
  askPublisherResources = Reader.asks publisherResources
