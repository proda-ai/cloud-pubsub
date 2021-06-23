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
import qualified Cloud.PubSub.Logger           as Logger
import qualified Cloud.PubSub.Publisher.Types  as PublisherT
import qualified Cloud.PubSub.Topic            as Topic
import qualified Cloud.PubSub.Topic.Types      as TopicT
import qualified Cloud.PubSub.Trans            as PubSubTrans
import           Control.Concurrent             ( threadDelay )
import qualified Control.Concurrent.MVar       as MVar
import qualified Control.Concurrent.STM        as STM
import           Control.Concurrent.STM         ( STM )
import           Control.Concurrent.STM.TVar    ( TVar )
import qualified Control.Concurrent.STM.TVar   as TVar
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
import qualified Control.Monad.Logger          as ML
import           Data.Aeson                     ( (.=)
                                                , object
                                                )
import qualified Data.DList                    as DList
import qualified Data.Map.Strict               as Map
import qualified Data.Time                     as Time

notifySentMsgIds
  :: Logger.LoggerFn -> [PublisherT.PendingMessageBatch] -> [MessageId] -> IO ()
notifySentMsgIds _      []       _      = return ()
notifySentMsgIds logger (x : xs) msgIds = do
  flip ML.runLoggingT logger
    $ Logger.logWithContext Logger.Debug ctx "Notifying batch publish success"
  MVar.putMVar (PublisherT.status x) (PublisherT.Sent batchIds)
  notifySentMsgIds logger xs rest
 where
  batchSize        = PublisherT.pendingMessageBatchSize x
  (batchIds, rest) = splitAt batchSize msgIds
  ctx =
    Just $ object ["batchId" .= PublisherT.batchId x, "batchSize" .= batchSize]

notifyPublishErr
  :: Logger.LoggerFn
  -> TopicName
  -> SomeException
  -> PublisherT.PendingMessageBatch
  -> IO ()
notifyPublishErr logger topicName e b = do
  let failCtx =
        Just $ object ["batchId" .= PublisherT.batchId b, "topic" .= topicName]
  flip ML.runLoggingT logger
    $ Logger.logWithContext Logger.Error failCtx "Notifying of publish failure"
  MVar.putMVar (PublisherT.status b) (PublisherT.Failed e)

readBatch
  :: PublisherT.PublisherConfig
  -> Time.UTCTime
  -> TVar PublisherT.TopicBatches
  -> IO PublisherT.TopicBatches
readBatch config readTime topicBatchesTVar = STM.atomically
  $ TVar.stateTVar topicBatchesTVar (Map.partition decideBatch)
 where
  decideBatch tb =
    let timeSinceBatchCreated =
          Time.diffUTCTime readTime $ PublisherT.tbOldestBatch tb
    in  (timeSinceBatchCreated >= maxDelay)
          || (PublisherT.tbTotal tb >= maxBatchSize)
  maxDelay     = PublisherT.maxBatchDelay config
  maxBatchSize = PublisherT.maxBatchSize config

workerLoop
  :: PublisherT.PublisherConfig
  -> Logger.LoggerFn
  -> PublisherT.PublisherImpl
  -> TVar PublisherT.TopicBatches
  -> IO ()
workerLoop config logger publisherImpl topicBatchesTVar = do
  now      <- Time.getCurrentTime
  consumed <- readBatch config now topicBatchesTVar
  forM_ (Map.toList consumed) $ \(topic, tb) -> do
    let pmbs             = DList.toList $ PublisherT.tbBatches tb
        batchIds         = map PublisherT.batchId pmbs
        pubCtx = Just $ object ["batchIds" .= batchIds, "topic" .= topic]
        combinedMessages = concatMap PublisherT.messages pmbs
    flip ML.runLoggingT logger
      $ Logger.logWithContext Logger.Debug pubCtx "Publishing messages batches"
    try (PublisherT.publish publisherImpl topic combinedMessages) >>= \case
      Right msgIds -> do
        notifySentMsgIds logger pmbs msgIds
      Left e -> do
        liftIO $ mapM_ (notifyPublishErr logger topic e) pmbs
  threadDelay (1000 * 10) -- to avoid spinning wait 10 millis
  workerLoop config logger publisherImpl topicBatchesTVar

extractMessage :: Message -> TopicT.PublishPubsubMessage
extractMessage m = TopicT.PublishPubsubMessage
  { ppmOrderingKey = key m
  , ppmData        = Base64DataString $ value m
  }

mkPublisherImpl
  :: Logger.LoggerFn -> PubSubTrans.PubSubEnv -> PublisherT.PublisherImpl
mkPublisherImpl logger env = PublisherT.PublisherImpl
  { publish = \topicName msgs ->
                PubSubTrans.runPubSubT logger env
                  $ Topic.publish topicName
                  $ map extractMessage msgs
  }

mkPublisherResources
  :: Logger.LoggerFn
  -> PublisherT.PublisherImpl
  -> PublisherT.PublisherConfig
  -> IO PublisherT.PublisherResources
mkPublisherResources logger publisherImpl config = do
  topicBatchesTVar <- TVar.newTVarIO Map.empty
  workerId         <- Immortal.create $ const $ workerLoop config
                                                           logger
                                                           publisherImpl
                                                           topicBatchesTVar
  return $ PublisherT.PublisherResources topicBatchesTVar config workerId

closePublisherResources :: PublisherT.PublisherResources -> IO ()
closePublisherResources = Immortal.stop . PublisherT.worker

mkPendingMessageBatch
  :: PublisherT.BatchId
  -> TopicName
  -> [Message]
  -> IO PublisherT.PendingMessageBatch
mkPendingMessageBatch bId topicName messages = do
  now        <- Time.getCurrentTime
  statusMVar <- MVar.newEmptyMVar
  return $ PublisherT.PendingMessageBatch { batchId     = bId
                                          , enqueueTime = now
                                          , topic       = topicName
                                          , messages    = messages
                                          , status      = statusMVar
                                          }

enqueue
  :: Int
  -> TVar PublisherT.TopicBatches
  -> PublisherT.PendingMessageBatch
  -> STM ()
enqueue maxQueueMsgs topicBatchesTVar batch = do
  topicBatches        <- TVar.readTVar topicBatchesTVar
  updatedTopicBatches <- Map.alterF addBatch
                                    (PublisherT.topic batch)
                                    topicBatches
  TVar.writeTVar topicBatchesTVar updatedTopicBatches
 where
  batchSize = PublisherT.pendingMessageBatchSize batch
  addBatch Nothing = return $ Just $ PublisherT.newTopicBatch batch
  addBatch (Just topicBatch) =
    if PublisherT.tbTotal topicBatch + batchSize <= maxQueueMsgs
      then
        let updatedTopicBatch = PublisherT.addToTopicBatch batch topicBatch
        in  return $ Just updatedTopicBatch
      else STM.retry

publishAsync
  :: ( PublisherT.HasPublisherResources m
     , ML.MonadLogger m
     , MonadThrow m
     , MonadIO m
     )
  => TopicName
  -> [Message]
  -> m PublisherT.PublishResult
publishAsync topicName messages = do
  batchId   <- PublisherT.genBatchId
  resources <- PublisherT.askPublisherResources
  let pendingTVar  = PublisherT.pendingTopicBatches resources
      config       = PublisherT.publisherConfig resources
      maxBatchSize = PublisherT.maxBatchSize config
      ctx          = Just $ object ["topic" .= topicName, "batchId" .= batchId]

  if batchMessageCount <= maxBatchSize
    then do
      pendingMessages <- liftIO
        $ mkPendingMessageBatch batchId topicName messages
      Logger.logWithContext Logger.Debug ctx "Enqueueing batch"
      liftIO $ STM.atomically $ enqueue maxBatchSize pendingTVar pendingMessages
      Logger.logWithContext Logger.Debug ctx "Enqueued batch"
      let status = PublisherT.status pendingMessages
      return $ PublisherT.PublishResult topicName batchId status
    else throwM $ PublisherT.MessageBatchSizeExceeded
      { messageCount = batchMessageCount
      , maxBatch     = maxBatchSize
      }
  where batchMessageCount = length messages

waitForPublishResult
  :: (ML.MonadLogger m, MonadIO m, MonadThrow m)
  => PublisherT.PublishResult
  -> m [MessageId]
waitForPublishResult result = do
  Logger.logWithContext Logger.Debug ctx "Waiting for messages to be published"
  liftIO (MVar.readMVar (PublisherT.prStatus result)) >>= \case
    PublisherT.Sent ids -> do
      Logger.logWithContext Logger.Debug ctx "Batch published sucessfully"
      return ids
    PublisherT.Failed e -> do
      Logger.logWithContext Logger.Error ctx "Failed to publish batch"
      throwM e
  where ctx = Just $ PublisherT.publishResultCtx result

publishSync
  :: ( PublisherT.HasPublisherResources m
     , ML.MonadLogger m
     , MonadIO m
     , MonadThrow m
     )
  => TopicName
  -> [Message]
  -> m [MessageId]
publishSync topicName messages =
  publishAsync topicName messages >>= waitForPublishResult
