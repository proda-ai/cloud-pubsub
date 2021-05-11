module Cloud.PubSub.SnapshotSpec where

import           Cloud.PubSub.Core.Types        ( UpdateMask(..) )
import qualified Cloud.PubSub.Http.Types       as HttpT
import qualified Cloud.PubSub.Snapshot         as Snapshot
import qualified Cloud.PubSub.Snapshot.Types   as SnapshotT
import           Cloud.PubSub.TestHelpers       ( mkTestPubSubEnv
                                                , runTest
                                                , withTestSnapshot
                                                , withTestTopicAndSub
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.HashMap.Strict           as HM
import           Test.Hspec
import qualified Cloud.PubSub.IO               as PubSubIO

snapshotManagementTest :: PubSubIO.PubSubEnv -> IO ()
snapshotManagementTest = runTest $ withTestTopicAndSub topicName subName $ do
  Snapshot.delete snapshotName
  Right createdSnap <- Snapshot.createWithDefaults snapshotName subName
  let snapshotPatch = SnapshotT.SnapshotPatch
        { snapshot   = createdSnap
          { SnapshotT.labels = Just $ HM.fromList [("patched", "successful")]
          }
        , updateMask = UpdateMask "labels"
        }
  updatedSnap <- Snapshot.patch snapshotName snapshotPatch
  fetchedSnap <- Snapshot.get snapshotName
  Snapshot.delete snapshotName
  liftIO $ fetchedSnap `shouldBe` updatedSnap
 where
  topicName    = "snapshot-management-test"
  subName      = "snapshot-management-test"
  snapshotName = "snapshot-management-test"

duplicateSnapshotTest :: PubSubIO.PubSubEnv -> IO ()
duplicateSnapshotTest =
  runTest
    $ withTestTopicAndSub topicName subName
    $ withTestSnapshot snapshotName subName
    $ do
        result <- Snapshot.createWithDefaults snapshotName subName
        liftIO $ result `shouldBe` Left SnapshotT.SnapshotAlreadyExists
 where
  topicName    = "snapshot-duplicates-test"
  subName      = "snapshot-duplicates-test"
  snapshotName = "snapshot-duplicates-test"

snapshotListTest :: PubSubIO.PubSubEnv -> IO ()
snapshotListTest =
  runTest
    $ withTestTopicAndSub topicName subName
    $ withTestSnapshot snapshotName subName
    $ do
        fetchedSnaps <- Snapshot.list Nothing
        projectId    <- HttpT.askProjectId
        let fetchedSnapNames =
              map SnapshotT.name $ SnapshotT.snapshots fetchedSnaps
            qualify = SnapshotT.qualifySnapshotName projectId
        liftIO $ do
          fetchedSnapNames `shouldContain` [qualify snapshotName]
 where
  topicName    = "snapshost-list-test"
  subName      = "snapshost-list-test"
  snapshotName = "snapshost-list-test"

snapshotPaginatedListTest :: PubSubIO.PubSubEnv -> IO ()
snapshotPaginatedListTest =
  runTest
    $ withTestTopicAndSub topicName subName
    $ withTestSnapshot snapshot1 subName
    $ withTestSnapshot snapshot2 subName
    $ do
        fetchedSnapshots1 <- Snapshot.list $ Just $ HttpT.PageQuery 1 Nothing
        let nextToken = SnapshotT.nextPageToken fetchedSnapshots1
        fetchedSnapshots2 <- Snapshot.list $ Just $ HttpT.PageQuery 1 nextToken
        -- we only know that there are at least two Snapshots, given
        -- that there could be many others we only see if the params
        -- work
        liftIO $ do
          SnapshotT.snapshots fetchedSnapshots1
            `shouldSatisfy` ((== 1) . length)
          SnapshotT.snapshots fetchedSnapshots2
            `shouldSatisfy` ((== 1) . length)
 where
  topicName = "snapshost-paginated-list-test"
  subName   = "snapshost-paginated-list-test"
  snapshot1 = "snapshost-paginated-1-test"
  snapshot2 = "snapshost-paginated-2-test"

spec :: Spec
spec = parallel $ before mkTestPubSubEnv $ do
  describe "Snapshot Endpoints" $ do
    it "can create/patch/get/delete a snapshot"         snapshotManagementTest
    it "returns an error if the snapshot name is taken" duplicateSnapshotTest
    it "can list snapshots"                             snapshotListTest
    it "can list snapshots with pagination" snapshotPaginatedListTest
