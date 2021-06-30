module Cloud.PubSub.PublisherSpec where

import qualified Cloud.PubSub.Core.Types       as CoreT
import qualified Cloud.PubSub.Publisher        as Publisher
import qualified Cloud.PubSub.Publisher.Trans  as PublisherTrans
import qualified Cloud.PubSub.Publisher.Types  as PublisherT
import qualified Cloud.PubSub.Subscription     as Subscription
import qualified Cloud.PubSub.Subscription.Types
                                               as SubscriptionT
import           Cloud.PubSub.TestHelpers       ( TestEnv(..)
                                                , convertMessage
                                                , logger
                                                , mkTestPubSubEnv
                                                , withTestSub
                                                , withTestTopic
                                                )
import qualified Cloud.PubSub.Trans            as PubSubTrans
import           Control.Monad.Catch            ( bracket )
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.ByteString.Char8         as C8
import           Test.Hspec

publisherConfig :: PublisherT.PublisherConfig
publisherConfig =
  PublisherT.PublisherConfig { maxBatchSize = 20, maxBatchDelay = 0.1 }


runTest :: PublisherTrans.PublisherT IO a -> TestEnv -> IO a
runTest action (TestEnv env@(PubSubTrans.PubSubEnv envClientResources)) = do
  bracket acquire release $ \pubResources ->
    let publishEnv =
          PublisherTrans.PublisherEnv envClientResources pubResources
    in  PublisherTrans.runPublisherT logger publishEnv action
 where
  publisherImpl = Publisher.mkPublisherImpl logger env
  acquire = Publisher.mkPublisherResources logger publisherImpl publisherConfig
  release = Publisher.closePublisherResources

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
