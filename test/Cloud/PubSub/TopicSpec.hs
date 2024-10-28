{-# LANGUAGE OverloadedLists #-}

module Cloud.PubSub.TopicSpec where

import qualified Cloud.PubSub.Core.Types       as Core
import qualified Cloud.PubSub.Http.Types       as HttpT
import           Cloud.PubSub.TestHelpers       ( TestEnv
                                                , mkTestPubSubEnv
                                                , runTest
                                                , runTestIfNotEmulator
                                                , withTestTopic
                                                )
import qualified Cloud.PubSub.Topic            as Topic
import qualified Cloud.PubSub.Topic.Types      as TopicT
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Functor                   ( void )
import qualified Data.HashMap.Strict           as HM
import           Test.Hspec

basicTopicManagementTest :: TestEnv -> IO ()
basicTopicManagementTest = runTest $ do
  Topic.delete topic
  Right createdTopic <- Topic.createWithDefaults topic
  fetchedTopic       <- Topic.get topic
  Topic.delete topic
  liftIO $ createdTopic `shouldBe` fetchedTopic
  where topic = "topic-management-test"

topicUpdateTest :: TestEnv -> IO ()
topicUpdateTest = runTestIfNotEmulator $ withTestTopic topic $ do
  initialTopic <- Topic.get topic
  let topicPatch = TopicT.TopicPatch
        { topic      = initialTopic
          { TopicT.labels = Just $ HM.fromList [("patched", "successful")]
          }
        , updateMask = Core.UpdateMask "labels"
        }
  updatedTopic <- Topic.patch topic topicPatch
  fetchedTopic <- Topic.get topic
  liftIO $ updatedTopic `shouldBe` fetchedTopic
  where topic = "topic-update-test"

duplicateTopicTest :: TestEnv -> IO ()
duplicateTopicTest = runTest $ withTestTopic topic $ do
  result <- Topic.createWithDefaults topic
  liftIO $ result `shouldBe` Left TopicT.TopicAlreadyExists
  where topic = "topic-duplicates-test"

topicPublishTest :: TestEnv -> IO ()
topicPublishTest = runTest $ withTestTopic topic $ do
  void $ Topic.publish topic [message]
 where
  topic   = "topic-publish-test"
  message = TopicT.PublishPubsubMessage { ppmOrderingKey = Just "constant-key"
                                        , ppmData        = "some data"
                                        , ppmAttributes  = Just [("attribute-key", "attribute-value")]
                                        }

topicListTest :: TestEnv -> IO ()
topicListTest = runTest $ withTestTopic topic $ do
  fetchedTopics <- Topic.list Nothing
  projectId     <- HttpT.askProjectId
  let fetchedTopicNames = map TopicT.name $ TopicT.topics fetchedTopics
      qualify           = Core.qualifyTopicName projectId
  liftIO $ fetchedTopicNames `shouldContain` [qualify topic]
  where topic = "topic-list-test"

topicPaginiatedListTest :: TestEnv -> IO ()
topicPaginiatedListTest =
  runTest $ withTestTopic topic1 $ withTestTopic topic2 $ do
    fetchedTopics1 <- Topic.list $ Just $ HttpT.PageQuery 1 Nothing
    let nextToken = TopicT.nextPageToken fetchedTopics1
    fetchedTopics2 <- Topic.list $ Just $ HttpT.PageQuery 1 nextToken
    -- we only know that there are at least two topics, given
    -- that there could be many others we only see if the params
    -- work
    liftIO $ do
      TopicT.topics fetchedTopics1 `shouldSatisfy` ((== 1) . length)
      TopicT.topics fetchedTopics2 `shouldSatisfy` ((== 1) . length)
 where
  topic1 = "topic-1-list-test"
  topic2 = "topic-2-list-test"

spec :: Spec
spec = parallel $ before mkTestPubSubEnv $ do
  describe "Topic Endpoints" $ do
    it "can create/get/delete a topic"               basicTopicManagementTest
    it "can patch a topic"                           topicUpdateTest
    it "returns an error if the topic name is taken" duplicateTopicTest
    it "can publish to a topic"                      topicPublishTest
    it "can list topics"                             topicListTest
    it "can list topics with pagination"             topicPaginiatedListTest
