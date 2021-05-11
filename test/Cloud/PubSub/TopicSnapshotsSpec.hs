module Cloud.PubSub.TopicSnapshotsSpec where

import qualified Cloud.PubSub.Http.Types       as HttpT
import qualified Cloud.PubSub.IO               as PubSubIO
import qualified Cloud.PubSub.Snapshot.Types   as SnapshotT
import           Cloud.PubSub.TestHelpers       ( mkTestPubSubEnv
                                                , runTest
                                                , withTestSnapshot
                                                , withTestTopicAndSub
                                                )
import qualified Cloud.PubSub.TopicSnapshots   as TopicSnapshots
import           Control.Monad.IO.Class         ( liftIO )
import           Test.Hspec

topicSubscriptionListTest :: PubSubIO.PubSubEnv -> IO ()
topicSubscriptionListTest =
  runTest
    $ withTestTopicAndSub topicName subName
    $ withTestSnapshot snapName subName
    $ do
        fetchedSubNames <-
          TopicSnapshots.snapshots <$> TopicSnapshots.list topicName Nothing
        projectId <- HttpT.askProjectId
        let qualify = SnapshotT.qualifySnapshotName projectId
        liftIO $ fetchedSubNames `shouldContain` [qualify snapName]
 where
  topicName = "topic-snapshot-list-test"
  subName   = "topic-snapshot-list-test"
  snapName  = "topic-snapshot-list-test"

spec :: Spec
spec = parallel $ before mkTestPubSubEnv $ do
  describe "Topic Snapshots Endpoints" $ do
    it "can list topic snapshots" topicSubscriptionListTest
