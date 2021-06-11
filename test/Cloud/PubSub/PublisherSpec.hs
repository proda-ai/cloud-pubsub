module Cloud.PubSub.PublisherSpec where

import qualified Cloud.PubSub.Core.Types       as CoreT
import qualified Cloud.PubSub.IO               as PubSubIO
import qualified Cloud.PubSub.Publisher        as Publisher
import qualified Cloud.PubSub.Publisher.IO     as PublisherIO
import qualified Cloud.PubSub.Publisher.Types  as PublisherT
import qualified Cloud.PubSub.Subscription     as Subscription
import qualified Cloud.PubSub.Subscription.Types
                                               as SubscriptionT
import           Cloud.PubSub.TestHelpers       ( TestEnv(..)
                                                , convertMessage
                                                , mkTestPubSubEnv
                                                , withTestSub
                                                , withTestTopic
                                                )
import           Control.Monad.Catch            ( bracket )
import           Control.Monad.IO.Class         ( liftIO )
import qualified Control.Monad.Reader          as Reader
import qualified Data.ByteString.Char8         as C8
import           Test.Hspec

publisherConfig :: PublisherT.PublisherConfig
publisherConfig = PublisherT.PublisherConfig { maxQueueMessageSize = 100
                                             , maxBatchSize        = 20
                                             , maxBatchDelay       = 0.1
                                             }


runTest :: PublisherIO.PublisherIO a -> TestEnv -> IO a
runTest action (TestEnv env) = do
  bracket acquire release $ \pubResources ->
    let publishEnv =
          PublisherIO.PublisherEnv envLogger envClientResources pubResources
    in  Reader.runReaderT (PublisherIO.runPublisherIO action) publishEnv
 where
  publisherImpl = Publisher.mkPublisherImpl env
  acquire =
    Publisher.mkPublisherResources envLogger publisherImpl publisherConfig
  release = Publisher.closePublisherResources
  PubSubIO.PubSubEnv envLogger envClientResources = env

publishMessageBatchTest :: TestEnv -> IO ()
publishMessageBatchTest = runTest $ withTestTopic topic $ do
  let messageCount = 10
      messages =
        [ CoreT.Message (Just "constant-key") (C8.pack $ show (x :: Int))
        | x <- [1 .. messageCount]
        ]
  ids <- Publisher.publishSync topic messages
  liftIO $ length ids `shouldBe` messageCount
  where topic = "publish-batch-test"

publishAndConsumeTest :: TestEnv -> IO ()
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
