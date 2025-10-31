module Cloud.PubSub.TestHelpers where

import qualified Cloud.PubSub                  as PubSub
import           Cloud.PubSub.Core.Types        ( Message(..)
                                                , TopicName
                                                )
import qualified Cloud.PubSub.Core.Types       as Core
import qualified Cloud.PubSub.Http.Types       as HttpT
import qualified Cloud.PubSub.Logger           as Logger
import qualified Data.Aeson                    as Aeson
import qualified Cloud.PubSub.Snapshot         as Snapshot
import qualified Cloud.PubSub.Snapshot.Types   as SnapshotT
import qualified Cloud.PubSub.Snapshot.Types   as SnapshotT.NewSnapshot
                                                ( NewSnapshot(..) )
import qualified Cloud.PubSub.Subscription     as Subscription
import qualified Cloud.PubSub.Subscription.Types
                                               as SubscriptionT
import qualified Cloud.PubSub.Subscription.Types
                                               as SubscriptionT.NewSubscription
                                                ( NewSubscription(..) )
import qualified Cloud.PubSub.Topic            as Topic
import qualified Cloud.PubSub.Topic.Types      as TopicT
import qualified Cloud.PubSub.Topic.Types      as TopicT.NewTopic
                                                ( NewTopic(..) )
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
import qualified System.Directory             as SystemDir
import qualified System.Environment            as SystemEnv
import qualified System.IO                     as SystemIO
import           Test.Hspec                     ( pendingWith )

newtype TestEnv = TestEnv
  { pubSubEnv :: PubSubTrans.PubSubEnv
  }

isEmulated :: TestEnv -> Bool
isEmulated (TestEnv (PubSubTrans.PubSubEnv cr)) =
  case HttpT.crTargetResorces cr of
    HttpT.Emulator -> True
    HttpT.Cloud _  -> False

testLabels :: HM.HashMap Text Text
testLabels = HM.fromList [("environment", "test")]

createTestTopic :: HttpT.PubSubHttpClientM m => TopicName -> m ()
createTestTopic topicName = Topic.create topicName newTopic >>= \case
  Right _ -> return ()
  Left TopicT.TopicAlreadyExists ->
    error $ "unexpected existing topic " <> show topicName
 where
  newTopic :: TopicT.NewTopic
  newTopic = TopicT.minimalNewTopic { TopicT.NewTopic.labels = Just testLabels
                                    }

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
        { SubscriptionT.NewSubscription.labels = Just testLabels
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
        { SnapshotT.NewSnapshot.labels = Just testLabels
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
  , attributes = SubscriptionT.pmAttributes m
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
  case maybeEmulatorHost of
    Just hostAndPortStr ->
      return $ PubSub.EmulatorTarget $ PubSub.HostAndPort hostAndPortStr
    Nothing -> do
      -- Check for GOOGLE_APPLICATION_CREDENTIALS (could be service account or ADC)
      maybeCredFile <- SystemEnv.lookupEnv "GOOGLE_APPLICATION_CREDENTIALS"
      case maybeCredFile of
        Just credFile -> do
          -- Try to detect if it's ADC or service account by parsing JSON
          credJSON <- Aeson.eitherDecodeFileStrict credFile >>= \case
            Left _ -> return Nothing
            Right v -> return $ Just v
          case credJSON >>= Aeson.lookup "type" of
            Just (Aeson.String "authorized_user") ->
              let authMethod = PubSub.ApplicationDefaultCredentialsFile credFile
              in  return
                  $ PubSub.CloudServiceTarget
                  $ PubSub.CloudConfig renewThreshold authMethod
            _ ->
              let authMethod = PubSub.ServiceAccountFile credFile
                 in  return
                     $ PubSub.CloudServiceTarget
                     $ PubSub.CloudConfig renewThreshold authMethod
        Nothing -> do
          -- Try default ADC location if GOOGLE_APPLICATION_CREDENTIALS not set
          maybeHome <- SystemEnv.lookupEnv "HOME"
          case maybeHome of
            Just homeDir -> do
              let defaultADC = homeDir <> "/.config/gcloud/application_default_credentials.json"
              adcExists <- SystemDir.doesFileExist defaultADC
              if adcExists
                then let authMethod = PubSub.ApplicationDefaultCredentialsFile defaultADC
                     in  return
                         $ PubSub.CloudServiceTarget
                         $ PubSub.CloudConfig renewThreshold authMethod
                else -- Fall back to metadata server (for GCP VMs)
                  let authMethod = PubSub.MetadataServer
                  in  return
                      $ PubSub.CloudServiceTarget
                      $ PubSub.CloudConfig renewThreshold authMethod
            Nothing -> -- Fall back to metadata server (for GCP VMs)
              let authMethod = PubSub.MetadataServer
              in  return
                  $ PubSub.CloudServiceTarget
                  $ PubSub.CloudConfig renewThreshold authMethod

usageMessage :: String
usageMessage = unlines
  [ "Missing config: Tests can be run against the hosted Cloud PubSub service \
    \or the emulator. The emulator does not have full API coverage and as such \
    \some tests are not run when the tests are with the emulator."
  , "To run tests with the hosted Cloud PubSub:"
  , "  Option 1 (recommended for local): Run 'gcloud auth application-default login' \
    \then set \"PROJECT_ID\" environment variable"
  , "  Option 2 (legacy): Set \"GOOGLE_APPLICATION_CREDENTIALS\" to the path \
    \to service account keys in JSON format and \"PROJECT_ID\""
  , "  Option 3 (GCP VM/CI): Use metadata server authentication (default when \
    \GOOGLE_APPLICATION_CREDENTIALS is not set and ADC file doesn't exist)"
  , "To run against the emulator: Start the emulator and set \"PUBSUB_EMULATOR_HOST\" \
    \and \"PROJECT_ID\" environment variables."
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
