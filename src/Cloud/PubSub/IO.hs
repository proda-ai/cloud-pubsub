module Cloud.PubSub.IO where

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

newtype PubSubIO a = PubSubIO
  { runPubSubIO :: ReaderT PubSubEnv IO a }
  deriving newtype(
    Functor, Applicative, Monad, MonadFail,
    MonadIO, MonadThrow, MonadCatch, MonadMask,
    MonadReader PubSubEnv)

instance HttpT.HasClientResources PubSubIO where
  askClientResources = Reader.asks clientResources
instance HttpT.HasGoogleProjectId PubSubIO
instance HttpT.HasPubSubHttpManager PubSubIO

instance AuthT.GoogleApiAuth PubSubIO where
  getToken = AuthToken.getToken

instance Logger.HasLogger PubSubIO where
  askLogger = Reader.asks logger

runPubSubIOToIO :: PubSubEnv -> PubSubIO a -> IO a
runPubSubIOToIO resources action =
  Reader.runReaderT (runPubSubIO action) resources
