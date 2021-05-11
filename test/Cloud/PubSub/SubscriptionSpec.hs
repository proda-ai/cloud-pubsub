module Cloud.PubSub.SubscriptionSpec where

import qualified Cloud.PubSub.Core.Types       as Core
import qualified Cloud.PubSub.Http.Types       as HttpT
import qualified Cloud.PubSub.IO               as PubSubIO
import qualified Cloud.PubSub.Subscription     as Subscription
import qualified Cloud.PubSub.Subscription.Types
                                               as SubscriptionT
import           Cloud.PubSub.TestHelpers       ( mkTestPubSubEnv
                                                , runTest
                                                , withTestSub
                                                , withTestTopic
                                                , withTestTopicAndSub
                                                )
import qualified Cloud.PubSub.Topic            as Topic
import qualified Cloud.PubSub.Topic.Types      as Topic
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.HashMap.Strict           as HM
import           Data.Maybe                     ( fromJust )
import qualified Data.Time                     as Time

import           Test.Hspec

testMessage :: Topic.PublishPubsubMessage
testMessage = Topic.PublishPubsubMessage { ppmOrderingKey = Just "constant-key"
                                         , ppmData        = "some data"
                                         }

subscriptionManagementTest :: PubSubIO.PubSubEnv -> IO ()
subscriptionManagementTest = runTest $ withTestTopic topicName $ do
  Subscription.delete subName
  Right createdSub <- Subscription.createWithDefaults subName topicName
  let subPatch = SubscriptionT.SubscriptionPatch
        { subscription = createdSub
          { SubscriptionT.labels = Just
                                     $ HM.fromList [("patched", "successful")]
          }
        , updateMask   = Core.UpdateMask "labels"
        }
  updatedSub <- Subscription.patch subName subPatch
  fetchedSub <- Subscription.get subName
  Subscription.delete subName
  let
    pushConfig =
      ( fromJust
        $ SubscriptionT.pushConfig (updatedSub :: SubscriptionT.Subscription)
        )
        { SubscriptionT.attributes = Nothing
        }
    expected :: SubscriptionT.Subscription
    expected = updatedSub { SubscriptionT.pushConfig = Just pushConfig }
  liftIO $ fetchedSub `shouldBe` expected
 where
  topicName = "subscription-management-test"
  subName   = "subscription-management-test"

duplicateSubscriptionTest :: PubSubIO.PubSubEnv -> IO ()
duplicateSubscriptionTest =
  runTest $ withTestTopicAndSub topicName subName $ do
    result <- Subscription.createWithDefaults subName topicName
    liftIO $ result `shouldBe` Left SubscriptionT.SubscriptionAlreadyExists
 where
  topicName = "subscription-duplicates-test"
  subName   = "subscription-duplicates-test"

subscriptionListTest :: PubSubIO.PubSubEnv -> IO ()
subscriptionListTest = runTest $ withTestTopicAndSub topicName subName $ do
  fetchedSubs <- Subscription.list Nothing
  projectId   <- HttpT.askProjectId
  let fetchedSubNames =
        map SubscriptionT.name $ SubscriptionT.subscriptions fetchedSubs
      qualify = SubscriptionT.qualifySubName projectId
  liftIO $ fetchedSubNames `shouldContain` [qualify subName]
 where
  topicName = "subscription-list-test"
  subName   = "subscription-list-test"

subscriptionPaginatedListTest :: PubSubIO.PubSubEnv -> IO ()
subscriptionPaginatedListTest =
  runTest
    $ withTestTopic topic
    $ withTestSub topic sub1
    $ withTestSub topic sub2
    $ do
        fetchedSubscriptions1 <- Subscription.list $ Just $ HttpT.PageQuery
          1
          Nothing
        let nextToken = SubscriptionT.nextPageToken fetchedSubscriptions1
        fetchedSubscriptions2 <- Subscription.list $ Just $ HttpT.PageQuery
          1
          nextToken
        -- we only know that there are at least two subscriptions, given
        -- that there could be many others we only see if the params
        -- work
        liftIO $ do
          SubscriptionT.subscriptions fetchedSubscriptions1
            `shouldSatisfy` ((== 1) . length)
          SubscriptionT.subscriptions fetchedSubscriptions2
            `shouldSatisfy` ((== 1) . length)
 where
  topic = "subscription-paginated-list-test"
  sub1  = "subscription-1-list-test"
  sub2  = "subscription-2-list-test"

pullSubscriptionTest :: PubSubIO.PubSubEnv -> IO ()
pullSubscriptionTest = runTest $ withTestTopicAndSub topicName subName $ do
  _        <- Topic.publish topicName [testMessage]
  messages <- Subscription.pull subName 1
  liftIO $ length messages `shouldBe` 1
  let received = head messages
  Subscription.acknowledge subName [SubscriptionT.ackId received]
 where
  topicName = "subscription-pull-test"
  subName   = "subscription-pull-test"

seekSubscriptionTest :: PubSubIO.PubSubEnv -> IO ()
seekSubscriptionTest = runTest $ withTestTopicAndSub topicName subName $ do
  _   <- Topic.publish topicName [testMessage]
  now <- liftIO Time.getCurrentTime
  let target = SubscriptionT.TimeTarget now
  Subscription.seek subName target
 where
  topicName = "subscription-seek-test"
  subName   = "subscription-seek-test"

modifyAckSubscriptionTest :: PubSubIO.PubSubEnv -> IO ()
modifyAckSubscriptionTest =
  runTest $ withTestTopicAndSub topicName subName $ do
    _        <- Topic.publish topicName [testMessage]
    messages <- Subscription.pull subName 1
    liftIO $ length messages `shouldBe` 1
    let received = head messages
    Subscription.modifyAckDeadline subName [SubscriptionT.ackId received] 1
 where
  topicName = "subscription-modify-ack-test"
  subName   = "subscription-modify-ack-test"

detachSubscriptionTest :: PubSubIO.PubSubEnv -> IO ()
detachSubscriptionTest = runTest $ withTestTopicAndSub topicName subName $ do
  Subscription.detach subName
 where
  topicName = "subscription-detach-test"
  subName   = "subscription-detach-test"

spec :: Spec
spec = parallel $ before mkTestPubSubEnv $ do
  describe "Subscription Endpoints" $ do
    it "can create/patch/get/delete a subscription" subscriptionManagementTest
    it "returns an error if the subscription name is taken"
       duplicateSubscriptionTest
    it "can list subscriptions"                 subscriptionListTest
    it "can list subscriptions with pagination" subscriptionPaginatedListTest

    it "can pull message from a subscription and acknowledge it"
       pullSubscriptionTest
    it "can seek a subscription to a time" seekSubscriptionTest
    it "can modify the acknowledgement deadline for message"
       modifyAckSubscriptionTest
    it "can detach a subscription" detachSubscriptionTest
