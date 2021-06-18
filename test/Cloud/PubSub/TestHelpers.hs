module Cloud.PubSub.TestHelpers where

import qualified Cloud.PubSub                  as PubSub
import           Cloud.PubSub.Core.Types        ( Message(..)
                                                , TopicName
                                                )
import qualified Cloud.PubSub.Core.Types       as Core
import qualified Cloud.PubSub.Http.Types       as HttpT
import qualified Cloud.PubSub.Logger           as Logger
import qualified Cloud.PubSub.Snapshot         as Snapshot
import qualified Cloud.PubSub.Snapshot.Types   as SnapshotT
import qualified Cloud.PubSub.Subscription     as Subscription
import qualified Cloud.PubSub.Subscription.Types
                                               as SubscriptionT
import qualified Cloud.PubSub.Topic            as Topic
import qualified Cloud.PubSub.Topic.Types      as TopicT
import qualified Cloud.PubSub.Trans            as PubSubTrans
import qualified Control.Concurrent.MVar       as MVar
import           Control.Monad.Catch            ( Exception
                                                , MonadMask
                                                , bracket_
                                                )
import qualified Data.HashMap.Strict           as HM
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified System.Environment            as SystemEnv
import qualified System.IO                     as SystemIO
import           Test.Hspec                     ( pendingWith )

newtype TestEnv = TestEnv
  { pubSubEnv  :: PubSubTrans.PubSubEnv
  }

isEmulated :: TestEnv -> Bool
isEmulated (TestEnv (PubSubTrans.PubSubEnv _ cr)) =
  case HttpT.crTargetResorces cr of
    HttpT.Emulator -> True
    HttpT.Cloud _  -> False

-- This logger may inadvertently cause behaviour to change the mutex
-- may cause non-logging actions to synchronise through monadic
-- composition
createTestLogger :: IO Logger.Logger
createTestLogger = do
  mutex <- MVar.newMVar ()
  return $ Logger.Logger
    { logLevel   = Nothing
    , logMessage = MVar.withMVar mutex
                   . const
                   . SystemIO.hPutStrLn SystemIO.stderr
    }

testServiceAccountPath :: FilePath
testServiceAccountPath = "./secrets/service_account.json"

testLabels :: HM.HashMap Text Text
testLabels = HM.fromList [("environment", "test")]

createTestTopic :: HttpT.PubSubHttpClientM m => TopicName -> m ()
createTestTopic topicName = Topic.create topicName newTopic >>= \case
  Right _ -> return ()
  Left TopicT.TopicAlreadyExists ->
    error $ "unexpected existing topic " <> show topicName
 where
  newTopic :: TopicT.NewTopic
  newTopic = TopicT.minimalNewTopic { TopicT.labels = Just testLabels }

withTestTopic
  :: (HttpT.PubSubHttpClientM m, MonadMask m) => TopicName -> m a -> m a
withTestTopic topicName action = do
  bracket_ (Topic.delete topicName >> createTestTopic topicName)
           (Topic.delete topicName)
           action

createTestSub
  :: HttpT.PubSubHttpClientM m => SubscriptionT.SubName -> TopicName -> m ()
createTestSub subName topicName = do
  projectId <- HttpT.askProjectId
  let qualifiedTopic = Core.qualifyTopicName projectId topicName
      newSub :: SubscriptionT.NewSubscription
      newSub = (SubscriptionT.minimalNewSubscription qualifiedTopic)
        { SubscriptionT.labels = Just testLabels
        }
  Subscription.create subName newSub >>= \case
    Right _ -> return ()
    Left SubscriptionT.SubscriptionAlreadyExists ->
      error $ "unexpected existing subscription " <> show subName

withTestSub
  :: (HttpT.PubSubHttpClientM m, MonadMask m)
  => TopicName
  -> SubscriptionT.SubName
  -> m a
  -> m a
withTestSub topicName subName action = do
  bracket_ (Subscription.delete subName >> createTestSub subName topicName)
           (Subscription.delete subName)
           action

createSnapshot
  :: HttpT.PubSubHttpClientM m
  => SnapshotT.SnapshotName
  -> SubscriptionT.SubName
  -> m ()
createSnapshot snapshotName subName = do
  projectId <- HttpT.askProjectId
  let qualifiedSub = SubscriptionT.qualifySubName projectId subName
      newSnap :: SnapshotT.NewSnapshot
      newSnap = (SnapshotT.minimalNewSnapshot qualifiedSub)
        { SnapshotT.labels = Just testLabels
        }
  Snapshot.create snapshotName newSnap >>= \case
    Right _ -> return ()
    Left SnapshotT.SnapshotAlreadyExists ->
      error $ "unexpected existing snapshot " <> show snapshotName

withTestSnapshot
  :: (HttpT.PubSubHttpClientM m, MonadMask m)
  => SnapshotT.SnapshotName
  -> SubscriptionT.SubName
  -> m a
  -> m a
withTestSnapshot snapshotName subName action = do
  bracket_
    (Snapshot.delete snapshotName >> createSnapshot snapshotName subName)
    (Snapshot.delete snapshotName)
    action

withTestTopicAndSub
  :: (HttpT.PubSubHttpClientM m, MonadMask m)
  => TopicName
  -> SubscriptionT.SubName
  -> m a
  -> m a
withTestTopicAndSub topicName subName =
  withTestTopic topicName . withTestSub topicName subName

convertMessage :: SubscriptionT.PubsubMessage -> Message
convertMessage m = Message
  { key   = SubscriptionT.pmOrderingKey m
  , value = Core.unwrapBase64DataString (SubscriptionT.pmData m)
  }

runTest :: PubSubTrans.PubSubT IO a -> TestEnv -> IO a
runTest action env = PubSubTrans.runPubSubT (pubSubEnv env) action

runTestIfNotEmulator :: PubSubTrans.PubSubT IO () -> TestEnv -> IO ()
runTestIfNotEmulator action env = do
  if isEmulated env
    then pendingWith "skipping as action not supported in emulator"
    else PubSubTrans.runPubSubT (pubSubEnv env) action

mkTestPubSubEnv :: IO TestEnv
mkTestPubSubEnv = do
  projectId <- Core.ProjectId . Text.pack <$> SystemEnv.getEnv "PROJECT_ID"
  target    <- SystemEnv.lookupEnv "PUBSUB_EMULATOR_HOST" >>= \case
    Just hostPort -> return $ PubSub.EmulatorTarget $ PubSub.HostPort hostPort
    Nothing       -> do
      saFile <- SystemEnv.getEnv "GOOGLE_APPLICATION_CREDENTIALS"
      let authMethod = PubSub.ServiceAccountFile saFile
      return $ PubSub.CloudServiceTarget $ PubSub.CloudConfig 60 authMethod
  logger <- createTestLogger
  TestEnv <$> PubSub.mkPubSubEnv projectId target logger

data ExpectedError = ExpectedError
  deriving stock (Show, Eq)

instance Exception ExpectedError
