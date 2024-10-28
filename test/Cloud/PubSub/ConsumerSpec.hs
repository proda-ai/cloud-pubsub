{-# LANGUAGE OverloadedLists #-}

module Cloud.PubSub.ConsumerSpec where

import qualified Cloud.PubSub.Consumer         as Consumer
import qualified Cloud.PubSub.Subscription.Types
                                               as SubscriptionT
import           Cloud.PubSub.TestHelpers       ( TestEnv(..)
                                                , logger
                                                , mkTestPubSubEnv
                                                , runTest
                                                , withTestTopicAndSub
                                                )
import qualified Cloud.PubSub.Topic            as Topic
import qualified Cloud.PubSub.Topic.Types      as Topic
import qualified Cloud.PubSub.Trans            as PubSubTrans
import qualified Control.Concurrent.MVar       as MVar
import qualified Control.Immortal              as Immortal
import           Control.Monad.IO.Class         ( liftIO )

import           Test.Hspec

publishAndConsumeTest :: TestEnv -> IO ()
publishAndConsumeTest testEnv = flip runTest testEnv $ do
  let
    topicName = "polling-consumer-test"
    subName   = "test"
    key       = "constant-key"
    value     = "some data"
    attributes = Just [("attribute-key", "attribute-value")]
    message =
      Topic.PublishPubsubMessage { ppmOrderingKey = Just key, ppmData = value, ppmAttributes = attributes }
    consumerConfig =
      Consumer.ConsumerConfig { pollBatchSize = 50, pollingInterval = 0.1 }

  withTestTopicAndSub topicName subName $ do
    _ <- Topic.publish topicName [message]
    liftIO $ do
      receivedMVar <- MVar.newEmptyMVar
      let action =
            PubSubTrans.runPubSubT logger (pubSubEnv testEnv)
              $ Consumer.processMessages consumerConfig
                                         subName
                                         (liftIO . MVar.putMVar receivedMVar)
      workerId <- Immortal.create $ const action
      received <- MVar.takeMVar receivedMVar
      Immortal.stop workerId
      SubscriptionT.pmOrderingKey received `shouldBe` Just key
      SubscriptionT.pmData received `shouldBe` value
      SubscriptionT.pmAttributes received `shouldBe` attributes

spec :: Spec
spec = before mkTestPubSubEnv $ do
  describe "Consumer" $ do
    it "can poll messages" publishAndConsumeTest
