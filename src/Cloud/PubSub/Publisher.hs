module Cloud.PubSub.Publisher
  ( mkPublisherResources
  , mkPublisherImpl
  , publishAsync
  , publishSync
  , waitForPublishResult
  , closePublisherResources
  ) where

import           Cloud.PubSub.Core.Types        ( Base64DataString(..)
                                                , Message(..)
                                                , MessageId
                                                , TopicName
                                                )
import qualified Cloud.PubSub.IO               as PubSubIO
import qualified Cloud.PubSub.Publisher.Types  as PublisherT
import qualified Cloud.PubSub.Topic            as Topic
import qualified Cloud.PubSub.Topic.Types      as TopicT
import qualified Control.Concurrent.MVar       as MVar

import           Cloud.PubSub.Logger            ( Logger
                                                , verboseLog
                                                )
import qualified Cloud.PubSub.Logger           as Logger
import           Control.Concurrent             ( threadDelay )
import qualified Control.Concurrent.STM        as STM
import           Control.Concurrent.STM         ( STM )
import qualified Control.Concurrent.STM.TQueue as TQueue
import           Control.Concurrent.STM.TQueue  ( TQueue )
import qualified Control.Immortal              as Immortal
import           Control.Monad                  ( forM_ )
import           Control.Monad.Catch            ( MonadThrow
                                                , SomeException
                                                , throwM
                                                , try
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Aeson                     ( (.=)
                                                , object
                                                )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HM
import qualified Data.Time                     as Time

getBatchMessageCount :: PublisherT.PendingMessageBatch -> Int
getBatchMessageCount = length . PublisherT.messages

getBatchListSum :: [PublisherT.PendingMessageBatch] -> Int
getBatchListSum = sum . map getBatchMessageCount

type TopicBatchMap = HashMap TopicName [PublisherT.PendingMessageBatch]

readBatch
  :: TQueue PublisherT.PendingMessageBatch
  -> TopicBatchMap
  -> Int
  -> STM (PublisherT.BatchCapacity, Int, TopicBatchMap)
readBatch _ xs 0 = if HM.null xs
  then return (PublisherT.Empty, 0, xs)
  else return (PublisherT.Full, 0, xs)
readBatch queue existing remaining = TQueue.tryReadTQueue queue >>= \case
  Nothing -> if HM.null existing
    then return (PublisherT.Empty, remaining, existing)
    else return (PublisherT.HasRoom, remaining, existing)
  Just batch -> do
    let batchMessageCount = PublisherT.pendingMessageBatchSize batch
    if remaining >= batchMessageCount
      then
        let updated =
              HM.insertWith (++) (PublisherT.topic batch) [batch] existing
            updatedRemaining = remaining - batchMessageCount
        in  readBatch queue updated updatedRemaining
      else do
        TQueue.unGetTQueue queue batch
        return (PublisherT.Full, remaining, existing)

notifySentMsgIds
  :: Logger -> [PublisherT.PendingMessageBatch] -> [MessageId] -> IO ()
notifySentMsgIds _      []       _      = return ()
notifySentMsgIds logger (x : xs) msgIds = do
  verboseLog logger Logger.Debug ctx "Notifying batch publish success"
  MVar.putMVar (PublisherT.status x) (PublisherT.Sent batchIds)
  notifySentMsgIds logger xs rest
 where
  batchSize        = PublisherT.pendingMessageBatchSize x
  (batchIds, rest) = splitAt batchSize msgIds
  ctx =
    Just $ object ["batchId" .= PublisherT.batchId x, "batchSize" .= batchSize]

notifyPublishErr
  :: Logger
  -> TopicName
  -> SomeException
  -> PublisherT.PendingMessageBatch
  -> IO ()
notifyPublishErr logger topicName e b = do
  let failCtx =
        Just $ object ["batchId" .= PublisherT.batchId b, "topic" .= topicName]
  verboseLog logger Logger.Error failCtx "Notifying of publish failure"
  MVar.putMVar (PublisherT.status b) (PublisherT.Failed e)

workerLoop
  :: PublisherT.PublisherConfig
  -> Logger
  -> PublisherT.PublisherImpl
  -> TQueue PublisherT.PendingMessageBatch
  -> Int
  -> Time.UTCTime
  -> TopicBatchMap
  -> IO ()
workerLoop config logger publisherImpl queue remaining lastPublishTime topicBatchMap
  = do
    now <- Time.getCurrentTime
    (batchCapacity, updatedRemaining, updatedTopicBatchMap) <-
      liftIO $ STM.atomically $ readBatch queue topicBatchMap remaining
    let timeSinceLastPublish = Time.diffUTCTime now lastPublishTime
    if (  (timeSinceLastPublish >= maxDelay)
       && (batchCapacity /= PublisherT.Empty)
       )
       || (batchCapacity == PublisherT.Full)
    then
      do
        verboseLog
          logger
          Logger.Debug
          (Just $ object
            [ "batchCapacity" .= batchCapacity
            , "timeSinceLastPublish" .= timeSinceLastPublish
            ]
          )
          "Publishing batches"
        forM_ (HM.toList updatedTopicBatchMap)
          $ \(topicName, pendingMessageBatches) -> do
              let batchIds = map PublisherT.batchId pendingMessageBatches
                  pubCtx =
                    Just $ object ["batchIds" .= batchIds, "topic" .= topicName]
              verboseLog logger
                         Logger.Debug
                         pubCtx
                         "Publishing messages batches"
              let combinedMessages =
                    concatMap PublisherT.messages pendingMessageBatches
              publishTime <- Time.getCurrentTime
              try (PublisherT.publish publisherImpl topicName combinedMessages)
                >>= \case
                      Right msgIds -> do
                        notifySentMsgIds logger pendingMessageBatches msgIds
                        workerLoop' maxBatchSize publishTime HM.empty
                      Left e -> do
                        liftIO $ mapM_ (notifyPublishErr logger topicName e)
                                       pendingMessageBatches
                        workerLoop' updatedRemaining
                                    lastPublishTime
                                    updatedTopicBatchMap
    else
      do
        verboseLog logger Logger.Debug Nothing "Sleeping and retrying"
        threadDelay (1000 * 50) -- to avoid spinning wait 50 millis
        workerLoop' updatedRemaining lastPublishTime updatedTopicBatchMap
 where
  workerLoop'  = workerLoop config logger publisherImpl queue
  maxDelay     = PublisherT.maxBatchDelay config
  maxBatchSize = PublisherT.maxBatchSize config

extractMessage :: Message -> TopicT.PublishPubsubMessage
extractMessage m = TopicT.PublishPubsubMessage
  { ppmOrderingKey = key m
  , ppmData        = Base64DataString $ value m
  }

mkPublisherImpl :: PubSubIO.PubSubEnv -> PublisherT.PublisherImpl
mkPublisherImpl env = PublisherT.PublisherImpl
  { publish = \topicName ->
                PubSubIO.runPubSubIOToIO env
                  . Topic.publish topicName
                  . map extractMessage
  }

mkPublisherResources
  :: Logger
  -> PublisherT.PublisherImpl
  -> PublisherT.PublisherConfig
  -> IO PublisherT.PublisherResources
mkPublisherResources logger publisherImpl config = do
  reqQueue <- TQueue.newTQueueIO
  now      <- Time.getCurrentTime
  workerId <- Immortal.create $ const $ workerLoop
    config
    logger
    publisherImpl
    reqQueue
    (PublisherT.maxBatchSize config)
    now
    HM.empty
  return $ PublisherT.PublisherResources reqQueue config workerId logger

closePublisherResources :: PublisherT.PublisherResources -> IO ()
closePublisherResources = Immortal.stop . PublisherT.worker

mkPendingMessageBatch
  :: PublisherT.BatchId
  -> TopicName
  -> [Message]
  -> IO PublisherT.PendingMessageBatch
mkPendingMessageBatch bId topicName messages = do
  statusMVar <- MVar.newEmptyMVar
  return $ PublisherT.PendingMessageBatch { batchId  = bId
                                          , topic    = topicName
                                          , messages = messages
                                          , status   = statusMVar
                                          }

enqueue
  :: Int
  -> TQueue PublisherT.PendingMessageBatch
  -> PublisherT.PendingMessageBatch
  -> STM ()
enqueue maxQueueMsgs queue batch = do
  reqs <- TQueue.flushTQueue queue
  let queueMsgCount = getBatchListSum reqs
  if queueMsgCount + getBatchMessageCount batch <= maxQueueMsgs
    then do
      mapM_ (TQueue.unGetTQueue queue) reqs
      TQueue.writeTQueue queue batch
    else STM.retry

publishAsync
  :: ( PublisherT.HasPublisherResources m
     , Logger.HasLogger m
     , MonadThrow m
     , MonadIO m
     )
  => TopicName
  -> [Message]
  -> m PublisherT.PublishResult
publishAsync topicName messages = do
  batchId   <- PublisherT.genBatchId
  resources <- PublisherT.askPublisherResources
  let queue        = PublisherT.pendingMessageBatchQueue resources
      config       = PublisherT.publisherConfig resources
      maxQueueMsgs = PublisherT.maxQueueMessageSize config
      maxBatchSize = PublisherT.maxBatchSize config
      ctx          = Just $ object ["topic" .= topicName, "batchId" .= batchId]

  if batchMessageCount <= maxBatchSize
    then do
      pendingMessages <- liftIO
        $ mkPendingMessageBatch batchId topicName messages
      Logger.log Logger.Debug ctx "Enqueueing batch"
      liftIO $ STM.atomically $ enqueue maxQueueMsgs queue pendingMessages
      let status = PublisherT.status pendingMessages
      return $ PublisherT.PublishResult topicName batchId status
    else throwM $ PublisherT.MessageBatchSizeExceeded
      { messageCount = batchMessageCount
      , maxBatch     = maxBatchSize
      }
  where batchMessageCount = length messages

waitForPublishResult
  :: (Logger.HasLogger m, MonadIO m, MonadThrow m)
  => PublisherT.PublishResult
  -> m [MessageId]
waitForPublishResult result = do
  Logger.log Logger.Debug ctx "Waiting for messages to be published"
  liftIO (MVar.readMVar (PublisherT.prStatus result)) >>= \case
    PublisherT.Sent ids -> do
      Logger.log Logger.Debug ctx "Batch published sucessfully"
      return ids
    PublisherT.Failed e -> do
      Logger.log Logger.Error ctx "Failed to publish batch"
      throwM e
  where ctx = Just $ PublisherT.publishResultCtx result

publishSync
  :: ( PublisherT.HasPublisherResources m
     , Logger.HasLogger m
     , MonadIO m
     , MonadThrow m
     )
  => TopicName
  -> [Message]
  -> m [MessageId]
publishSync topicName messages =
  publishAsync topicName messages >>= waitForPublishResult
