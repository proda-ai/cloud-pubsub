module Cloud.PubSub.Consumer
  ( ConsumerConfig(..)
  , processMessages
  ) where

import           Cloud.PubSub.Http.Types        ( PubSubHttpClientM )
import qualified Cloud.PubSub.Subscription     as Subscription
import           Cloud.PubSub.Subscription.Types
                                                ( SubName )
import qualified Cloud.PubSub.Subscription.Types
                                               as Subscription
import qualified Control.Concurrent            as Concurrent
import           Control.Monad                  ( forM
                                                , forever
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Time                      ( NominalDiffTime )
import qualified System.Clock                  as Clock

data ConsumerConfig = ConsumerConfig
  { pollBatchSize   :: Int
  , pollingInterval :: NominalDiffTime
  }

toDouble :: Real a => a -> Double
toDouble = realToFrac

epochMicros :: MonadIO m => m Int
epochMicros =
  liftIO
    $   round
    .   (/ 1000)
    .   (fromIntegral :: Integer -> Double)
    .   Clock.toNanoSecs
    <$> Clock.getTime Clock.Monotonic

runEvery :: MonadIO m => NominalDiffTime -> m () -> m ()
runEvery interval action = forever $ do
  start <- epochMicros
  action
  end <- epochMicros
  let elapsed  = end - start
      timeTill = round (1000000 * toDouble interval) - elapsed
  liftIO $ Concurrent.threadDelay timeTill

-- This fetches messages from subscription periodically, processes the messages
-- sequentially and acknowldeges the messages as processed.
-- Potential improvements 
-- * Mechanism to retry messages that could not be processed with a user 
-- provided strategy
-- * Provide a way to spawn a woerker running the process loop and provide
-- a way to terminate it
-- * Given that pubsub only gurantees ordered delivery of messages for each key
-- we have a variant that processes messages in parallel
processMessages
  :: PubSubHttpClientM m
  => ConsumerConfig
  -> SubName
  -> (Subscription.PubsubMessage -> m ())
  -> m ()
processMessages config subName process = runEvery samplingInterval $ do
  messages <- Subscription.pull subName batchSize
  ackIds   <- forM messages $ \rm -> do
    let message = Subscription.message rm
    process message
    return $ Subscription.ackId rm
  Subscription.acknowledge subName ackIds
 where
  batchSize        = pollBatchSize config
  samplingInterval = pollingInterval config
