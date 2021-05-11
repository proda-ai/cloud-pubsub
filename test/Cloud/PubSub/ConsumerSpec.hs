module Cloud.PubSub.ConsumerSpec where

import qualified Cloud.PubSub.Consumer         as Consumer
import qualified Cloud.PubSub.IO               as PubSubIO
import           Cloud.PubSub.TestHelpers       ( mkTestPubSubEnv
                                                , runTest
                                                , withTestTopicAndSub
                                                )
import qualified Cloud.PubSub.Topic            as Topic
import qualified Cloud.PubSub.Topic.Types      as Topic
import           Control.Monad.IO.Class         ( liftIO )
import           Test.Hspec

publishAndConsumeTest :: PubSubIO.PubSubEnv -> IO ()
publishAndConsumeTest = runTest $ do
  let topicName = "polling-consumer-test"
      subName   = "test"
      message   = Topic.PublishPubsubMessage
        { ppmOrderingKey = Just "constant-key"
        , ppmData        = "some data"
        }
      consumerConfig =
        Consumer.ConsumerConfig { pollBatchSize = 50, pollingInterval = 0.1 }

  withTestTopicAndSub topicName subName $ do
    _ <- Topic.publish topicName [message]
    Consumer.processMessages consumerConfig subName (liftIO . print)

spec :: Spec
spec = before mkTestPubSubEnv $ do
  describe "Stream Consumer" $ do
    xit "can poll messages" publishAndConsumeTest
