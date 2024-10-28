{-# LANGUAGE OverloadedLists #-}

module Cloud.PubSub.PublisherBatchingSpec where

import qualified Cloud.PubSub.Core.Types       as CoreT
import qualified Cloud.PubSub.Publisher        as Publisher
import qualified Cloud.PubSub.Publisher.Types  as PublisherT
import           Cloud.PubSub.TestHelpers       ( ExpectedError(..)
                                                , logger
                                                )
import qualified Control.Concurrent.STM        as STM
import qualified Control.Concurrent.STM.TVar   as TVar
import           Control.Concurrent.STM.TVar    ( TVar )
import           Control.Monad.Catch            ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                , bracket
                                                , throwM
                                                , try
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Control.Monad.Logger          as ML
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                )
import qualified Control.Monad.Reader          as Reader
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as C8
import           Data.Functor                   ( void )
import           Data.Text                      ( Text )
import qualified Data.Time                     as Time
import qualified Data.UUID                     as UUID
import qualified Data.UUID.V4                  as UUID
import           Test.Hspec

data PublishedMessage = PublishedMessage
  { messageId :: CoreT.MessageId
  , key       :: Maybe Text
  , value     :: ByteString
  }
  deriving (Show, Eq)

data PublishedBatch = PublishedBatch
  { topic         :: CoreT.TopicName
  , batchMessages :: [PublishedMessage]
  }
  deriving (Show, Eq)

newtype MockPublisherState = MockPublisherState
  { revPublishedBatches :: [PublishedBatch]
  } deriving (Show , Eq)

newtype MockPublisherEnv = MockPublisherEnv
  {  publisherResources :: PublisherT.PublisherResources
  }

newtype TestM a = TestM
  { runTestM :: ReaderT MockPublisherEnv (ML.LoggingT IO) a }
  deriving newtype(
    Functor, Applicative, Monad, MonadFail,
    MonadIO, MonadThrow, MonadCatch, MonadMask,
    MonadReader MockPublisherEnv, ML.MonadLogger)

instance PublisherT.HasPublisherResources TestM where
  askPublisherResources = Reader.asks publisherResources

runTest :: TestM a -> MockPublisherEnv -> IO a
runTest action env =
  ML.runLoggingT (Reader.runReaderT (runTestM action) env) logger

addMessage
  :: CoreT.TopicName
  -> [PublishedMessage]
  -> MockPublisherState
  -> MockPublisherState
addMessage tn ms state = MockPublisherState $ batch : revPublishedBatches state
  where batch = PublishedBatch tn ms

mkPublishedMessage :: CoreT.Message -> IO PublishedMessage
mkPublishedMessage m =
  PublishedMessage
    <$> (CoreT.MessageId . UUID.toText <$> UUID.nextRandom)
    <*> pure (CoreT.key m)
    <*> pure (CoreT.value m)

mockPublishMessage
  :: TVar MockPublisherState
  -> CoreT.TopicName
  -> [CoreT.Message]
  -> IO [CoreT.MessageId]
mockPublishMessage publisherStateVar topicName mesages = do
  publishedMessages <- mapM mkPublishedMessage mesages
  STM.atomically $ TVar.modifyTVar publisherStateVar
                                   (addMessage topicName publishedMessages)
  return $ map messageId publishedMessages

mkMessages :: [Int] -> [CoreT.Message]
mkMessages xs =
  [ CoreT.Message (Just "constant-key") (C8.pack $ show x) (Just [("attribute-key", "attribute-value")]) | x <- xs ]

mkPublisherConfigWithBatchSize :: Int -> PublisherT.PublisherConfig
mkPublisherConfigWithBatchSize mbs =
  PublisherT.PublisherConfig { maxBatchSize = mbs, maxBatchDelay = 0.5 }

mkEmptyMockPublisherState :: IO (TVar MockPublisherState)
mkEmptyMockPublisherState = TVar.newTVarIO $ MockPublisherState []

readMockMesssageBatches :: TVar MockPublisherState -> IO [PublishedBatch]
readMockMesssageBatches publisherStateVar = do
  state <- STM.atomically $ TVar.readTVar publisherStateVar
  return $ reverse $ revPublishedBatches state

withMockPublisherEnv
  :: PublisherT.PublisherConfig
  -> PublisherT.PublisherImpl
  -> (MockPublisherEnv -> IO a)
  -> IO a
withMockPublisherEnv publisherConfig publisherImpl f = do
  let acquire =
        Publisher.mkPublisherResources logger publisherImpl publisherConfig
  let release = Publisher.closePublisherResources
  bracket acquire release (f . MockPublisherEnv)

publishOnMaxBatchSizeTest :: IO ()
publishOnMaxBatchSizeTest = do
  publisherStateVar <- mkEmptyMockPublisherState
  let publisherImpl =
        PublisherT.PublisherImpl (mockPublishMessage publisherStateVar)
  messageIds <-
    withMockPublisherEnv publisherConfig publisherImpl $ runTest $ do
      firstBatchResult <- Publisher.publishAsync testTopic $ mkMessages [1 .. 5]
      secondBatchResult <- Publisher.publishAsync testTopic
        $ mkMessages [6 .. 8]
      (++)
        <$> Publisher.waitForPublishResult firstBatchResult
        <*> Publisher.waitForPublishResult secondBatchResult
  length messageIds `shouldBe` 8
  readMockMesssageBatches publisherStateVar >>= \case
    [batch1, batch2] -> do
      length (batchMessages batch1) `shouldBe` 5
      length (batchMessages batch2) `shouldBe` 3
    xs ->
      fail $ "expected two batches, got " <> show (length xs) <> " " <> show xs
 where
  testTopic       = "publish-on-max-batch-size-test"
  publisherConfig = mkPublisherConfigWithBatchSize 5

publishOnMaxDelayTest :: IO ()
publishOnMaxDelayTest = do
  publisherStateVar <- mkEmptyMockPublisherState
  let publisherImpl =
        PublisherT.PublisherImpl (mockPublishMessage publisherStateVar)
  start <- Time.getCurrentTime
  withMockPublisherEnv publisherConfig publisherImpl
    $ runTest
    $ void
    $ Publisher.publishSync testTopic messages
  end <- Time.getCurrentTime
  let elapsedTime = Time.diffUTCTime end start
  elapsedTime `shouldSatisfy` (\t -> 0.3 < t && t < 0.7)
  readMockMesssageBatches publisherStateVar >>= \case
    [batch] -> do
      length (batchMessages batch) `shouldBe` 4
    xs ->
      fail
        $  "expected a single batch, got "
        <> show (length xs)
        <> " "
        <> show xs
 where
  testTopic    = "publish-on-max-delay-test"
  messageCount = 4
  messages     = mkMessages [1 .. messageCount]
  publisherConfig =
    PublisherT.PublisherConfig { maxBatchSize = 5, maxBatchDelay = 0.5 }

opportunisticBatchingTest :: IO ()
opportunisticBatchingTest = do
  publisherStateVar <- mkEmptyMockPublisherState
  let publisherImpl =
        PublisherT.PublisherImpl (mockPublishMessage publisherStateVar)
  withMockPublisherEnv publisherConfig publisherImpl $ runTest $ do
    results <- mapM (\m -> Publisher.publishAsync testTopic [m]) messages
    mapM_ Publisher.waitForPublishResult results
  readMockMesssageBatches publisherStateVar >>= \case
    [batch1, batch2] -> do
      length (batchMessages batch1) `shouldBe` 5
      length (batchMessages batch2) `shouldBe` 5
    xs ->
      fail $ "expected two batches, got " <> show (length xs) <> " " <> show xs
 where
  testTopic       = "opportunistic-batching-test"
  messageCount    = 10
  messages        = mkMessages [1 .. messageCount]
  publisherConfig = mkPublisherConfigWithBatchSize 5

messageIdTest :: IO ()
messageIdTest = do
  publisherStateVar <- mkEmptyMockPublisherState
  let publisherImpl =
        PublisherT.PublisherImpl (mockPublishMessage publisherStateVar)
  messageIds <-
    withMockPublisherEnv publisherConfig publisherImpl $ runTest $ do
      results <- mapM (\m -> Publisher.publishAsync testTopic [m]) messages
      concat <$> mapM Publisher.waitForPublishResult results
  length messageIds `shouldBe` messageCount
  readMockMesssageBatches publisherStateVar >>= \case
    [batch] -> do
      length (batchMessages batch) `shouldBe` 10
      let expectedBatchMsgs = zipWith
            (\mId m -> PublishedMessage mId (CoreT.key m) (CoreT.value m))
            messageIds
            messages
      batchMessages batch `shouldMatchList` expectedBatchMsgs
    xs ->
      fail
        $  "expected a single batche, got "
        <> show (length xs)
        <> " "
        <> show xs
 where
  testTopic       = "message-id-test"
  messageCount    = 10
  messages        = mkMessages [1 .. messageCount]
  publisherConfig = mkPublisherConfigWithBatchSize 10

errorPropagationTest :: IO ()
errorPropagationTest = do
  let publisherImpl =
        PublisherT.PublisherImpl { publish = \_ _ -> throwM ExpectedError }
  withMockPublisherEnv publisherConfig publisherImpl $ runTest $ do
    try (Publisher.publishSync testTopic messages) >>= \case
      Right _ -> fail "expected an error to be raised"
      Left  e -> liftIO $ e `shouldBe` ExpectedError
 where
  testTopic       = "error-propagation-test"
  messages        = [CoreT.Message (Just "constant-key") "1" (Just [("attribute-key", "attribute-value")])]
  publisherConfig = mkPublisherConfigWithBatchSize 5

spec :: Spec
spec = do
  describe "Batching Behaviour" $ do
    it "publishes messages when batch size is reached" publishOnMaxBatchSizeTest
    it "publishes messages when the max delay has exceeded"
       publishOnMaxDelayTest
    it "can opportunistically batch messages" opportunisticBatchingTest
    it "returns the correct message ids for coalesced batches" messageIdTest
    it "propagates any publishing errors" errorPropagationTest
