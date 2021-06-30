module Cloud.PubSub.TopicSubscriptionsSpec where

import qualified Cloud.PubSub.Http.Types       as HttpT
import qualified Cloud.PubSub.Subscription.Types
                                               as SubscriptionT
import           Cloud.PubSub.TestHelpers       ( TestEnv
                                                , mkTestPubSubEnv
                                                , runTest
                                                , withTestTopicAndSub
                                                )
import qualified Cloud.PubSub.TopicSubscriptions
                                               as TopicSubscriptions
import           Control.Monad.IO.Class         ( liftIO )
import           Test.Hspec

topicSubscriptionListTest :: TestEnv -> IO ()
topicSubscriptionListTest =
  runTest $ withTestTopicAndSub topicName subName $ do
    fetchedSubNames <-
      TopicSubscriptions.subscriptions
        <$> TopicSubscriptions.list topicName Nothing
    projectId <- HttpT.askProjectId
    let qualify = SubscriptionT.qualifySubName projectId
    liftIO $ fetchedSubNames `shouldContain` [qualify subName]
 where
  topicName = "topic-subscription-list-test"
  subName   = "topic-subscription-list-test"

spec :: Spec
spec = parallel $ before mkTestPubSubEnv $ do
  describe "Topic Subscription Endpoints" $ do
    it "can list topic subscriptions" topicSubscriptionListTest
