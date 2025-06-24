module Cloud.PubSub.SnapshotSpec where

import           Cloud.PubSub.Core.Types        ( UpdateMask(..) )
import qualified Cloud.PubSub.Http.Types       as HttpT
import qualified Cloud.PubSub.Snapshot         as Snapshot
import qualified Cloud.PubSub.Snapshot.Types   as SnapshotT
import qualified Cloud.PubSub.Snapshot.Types   as SnapshotT.Snapshot
                                                ( Snapshot(..) )
import           Cloud.PubSub.TestHelpers       ( TestEnv
                                                , mkTestPubSubEnv
                                                , runTest
                                                , runTestIfNotEmulator
                                                , withTestSnapshot
                                                , withTestTopicAndSub
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.HashMap.Strict           as HM
import           Test.Hspec

basicSnapshotManagementTest :: TestEnv -> IO ()
basicSnapshotManagementTest =
  runTest $ withTestTopicAndSub topicName subName $ do
    Snapshot.delete snapshotName
    Right createdSnap <- Snapshot.createWithDefaults snapshotName subName
    fetchedSnap       <- Snapshot.get snapshotName
    Snapshot.delete snapshotName
    liftIO $ fetchedSnap `shouldBe` createdSnap
 where
  topicName    = "snapshot-management-test"
  subName      = "snapshot-management-test"
  snapshotName = "snapshot-management-test"

snapshotUpdateTest :: TestEnv -> IO ()
snapshotUpdateTest =
  runTestIfNotEmulator
    $ withTestTopicAndSub topicName subName
    $ withTestSnapshot snapshotName subName
    $ do
        initialSnap <- Snapshot.get snapshotName
        let
          snapshotPatch = SnapshotT.SnapshotPatch
            { snapshot   = initialSnap
              { SnapshotT.Snapshot.labels =
                  Just $ HM.fromList [("patched", "successful")]
              }
            , updateMask = UpdateMask "labels"
            }
        updatedSnap <- Snapshot.patch snapshotName snapshotPatch
        fetchedSnap <- Snapshot.get snapshotName
        liftIO $ fetchedSnap `shouldBe` updatedSnap

 where
  topicName    = "snapshot-update-test"
  subName      = "snapshot-update-test"
  snapshotName = "snapshot-update-test"

duplicateSnapshotTest :: TestEnv -> IO ()
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

snapshotListTest :: TestEnv -> IO ()
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

snapshotPaginatedListTest :: TestEnv -> IO ()
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
    it "can create/get/delete a snapshot" basicSnapshotManagementTest
    it "can patch a snapshot" snapshotUpdateTest
    it "returns an error if the snapshot name is taken" duplicateSnapshotTest
    it "can list snapshots" snapshotListTest
    it "can list snapshots with pagination" snapshotPaginatedListTest
