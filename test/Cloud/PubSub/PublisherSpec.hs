module Cloud.PubSub.PublisherSpec where

import qualified Cloud.PubSub.Auth.Token       as AuthToken
import qualified Cloud.PubSub.Auth.Types       as AuthT
import qualified Cloud.PubSub.Core.Types       as CoreT
import qualified Cloud.PubSub.Http.Types       as HttpT
import qualified Cloud.PubSub.IO               as PubSubIO
import qualified Cloud.PubSub.Logger           as Logger
import qualified Cloud.PubSub.Publisher        as Publisher
import qualified Cloud.PubSub.Publisher.Types  as PublisherT
import qualified Cloud.PubSub.Subscription     as Subscription
import qualified Cloud.PubSub.Subscription.Types
                                               as SubscriptionT
import           Cloud.PubSub.TestHelpers       ( convertMessage
                                                , mkTestPubSubEnv
                                                , withTestSub
                                                , withTestTopic
                                                )
import           Control.Monad.Catch            ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                , bracket
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Control.Monad.Reader          as Reader
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                )
import qualified Data.ByteString.Char8         as C8
import           Test.Hspec

publisherConfig :: PublisherT.PublisherConfig
publisherConfig = PublisherT.PublisherConfig { maxQueueMessageSize = 100
                                             , maxBatchSize        = 20
                                             , maxBatchDelay       = 0.1
                                             }

data PublisherEnv = PublisherEnv
  { logger             :: Logger.Logger
  , clientResources    :: HttpT.ClientResources
  , publisherResources :: PublisherT.PublisherResources
  }

newtype TestM a = TestM
  { runTestM :: ReaderT PublisherEnv IO a }
  deriving newtype(
    Functor, Applicative, Monad, MonadFail,
    MonadIO, MonadThrow, MonadCatch, MonadMask,
    MonadReader PublisherEnv)

instance HttpT.HasClientResources TestM where
  askClientResources = Reader.asks clientResources
instance HttpT.HasGoogleProjectId TestM
instance HttpT.HasPubSubHttpManager TestM

instance AuthT.GoogleApiAuth TestM where
  getToken = AuthToken.getToken

instance Logger.HasLogger TestM where
  askLogger = Reader.asks logger

instance PublisherT.HasPublisherResources TestM where
  askPublisherResources = Reader.asks publisherResources

runTest :: TestM a -> PubSubIO.PubSubEnv -> IO a
runTest action pubSubEnv@(PubSubIO.PubSubEnv envLogger envClientResources) = do
  bracket acquire release $ \pubResources ->
    let publishEnv = PublisherEnv envLogger envClientResources pubResources
    in  Reader.runReaderT (runTestM action) publishEnv
 where
  publisherImpl = Publisher.mkPublisherImpl pubSubEnv
  acquire =
    Publisher.mkPublisherResources envLogger publisherImpl publisherConfig
  release = Publisher.closePublisherResources

publishMessageBatchTest :: PubSubIO.PubSubEnv -> IO ()
publishMessageBatchTest = runTest $ withTestTopic topic $ do
  let messageCount = 10
      messages =
        [ CoreT.Message (Just "constant-key") (C8.pack $ show (x :: Int))
        | x <- [1 .. messageCount]
        ]
  ids <- Publisher.publishSync topic messages
  liftIO $ length ids `shouldBe` messageCount
  where topic = "publish-batch-test"

publishAndConsumeTest :: PubSubIO.PubSubEnv -> IO ()
publishAndConsumeTest = runTest $ withTestTopic topic $ do
  let message           = CoreT.Message (Just "constant-key") "hello"
      publishedMessages = [message]
  withTestSub topic subName $ do
    _                <- Publisher.publishSync topic publishedMessages
    receivedMessages <- Subscription.pull subName batchSize
    Subscription.acknowledge subName $ map SubscriptionT.ackId receivedMessages
    let messages =
          map (convertMessage . SubscriptionT.message) receivedMessages
    liftIO $ messages `shouldContain` publishedMessages
 where
  topic     = "publish-and-consume-test"
  subName   = "publish-and-consume-test"
  batchSize = 100

spec :: Spec
spec = before mkTestPubSubEnv $ do
  describe "Batching Publisher" $ do
    it "can publish a batch of messages"           publishMessageBatchTest
    it "can publish a message and then consume it" publishAndConsumeTest
