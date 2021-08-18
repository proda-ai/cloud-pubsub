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
import           Control.Monad.Catch            ( Exception
                                                , MonadMask
                                                , bracket_
                                                )
import qualified Control.Monad.Logger          as ML
import qualified Data.HashMap.Strict           as HM
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Time                      ( NominalDiffTime )
import qualified System.Environment            as SystemEnv
import qualified System.IO                     as SystemIO
import           Test.Hspec                     ( pendingWith )

newtype TestEnv = TestEnv
  { pubSubEnv  :: PubSubTrans.PubSubEnv
  }

isEmulated :: TestEnv -> Bool
isEmulated (TestEnv (PubSubTrans.PubSubEnv cr)) =
  case HttpT.crTargetResorces cr of
    HttpT.Emulator -> True
    HttpT.Cloud _  -> False

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

logger :: Logger.LoggerFn
logger = ML.defaultOutput SystemIO.stderr

runTest :: PubSubTrans.PubSubT IO a -> TestEnv -> IO a
runTest action env = PubSubTrans.runPubSubT logger (pubSubEnv env) action

runTestIfNotEmulator :: PubSubTrans.PubSubT IO () -> TestEnv -> IO ()
runTestIfNotEmulator action env = do
  if isEmulated env
    then pendingWith "skipping as action not supported in emulator"
    else PubSubTrans.runPubSubT logger (pubSubEnv env) action

getProjectId :: IO (Maybe Core.ProjectId)
getProjectId =
  fmap (Core.ProjectId . Text.pack) <$> SystemEnv.lookupEnv "PROJECT_ID"

getPubSubTarget :: NominalDiffTime -> IO PubSub.PubSubTarget
getPubSubTarget renewThreshold = do
  maybeEmulatorHost <- SystemEnv.lookupEnv "PUBSUB_EMULATOR_HOST"
  maybeSaFile       <- SystemEnv.lookupEnv "GOOGLE_APPLICATION_CREDENTIALS"
  case (maybeEmulatorHost, maybeSaFile) of
    (Just hostAndPortStr, Nothing) ->
      return $ PubSub.EmulatorTarget $ PubSub.HostAndPort hostAndPortStr
    (Nothing, Just saFile) ->
      let authMethod = PubSub.ServiceAccountFile saFile
      in  return $ PubSub.CloudServiceTarget $ PubSub.CloudConfig
            renewThreshold
            authMethod
    (_, _) ->
      error
        "Please specify either \"PUBSUB_EMULATOR_HOST\" or \
        \\"GOOGLE_APPLICATION_CREDENTIALS\" depending whether you to run \
        \the tests against the emulator or the cloud hosted version."

usageMessage :: String
usageMessage = unlines
  [ "Missing config: Tests can be run against the hosted Cloud PubSub service \
    \or the emulator. The emulator does not have full API coverage and as such \
    \some tests are not run when the tests are with the emulator."
  , "To run tests with the hosted Cloud PubSub, please set the enviroment \
    \variable \"GOOGLE_APPLICATION_CREDENTIALS\" to the path to the \
    \service account keys in JSON format and specify Google Cloud Project ID \
    \via the \"PROJECT_ID\" environment variable. Given that service accounts \
    \can be used across projects \"project_id\" field in the JSON key file \
    \is ignored."
  , "To run against the emulator please start the emulator and set the \
    \\"PUBSUB_EMULATOR_HOST\" and the \"PROJECT_ID\" environment variables."
  ]

mkTestPubSubEnvWithRenewThreshold :: NominalDiffTime -> IO TestEnv
mkTestPubSubEnvWithRenewThreshold renewThreshold = getProjectId >>= \case
  Nothing        -> error usageMessage
  Just projectId -> do
    target <- getPubSubTarget renewThreshold
    TestEnv <$> PubSub.mkPubSubEnv projectId target

mkTestPubSubEnv :: IO TestEnv
mkTestPubSubEnv = mkTestPubSubEnvWithRenewThreshold 60

data ExpectedError = ExpectedError
  deriving stock (Show, Eq)

instance Exception ExpectedError
