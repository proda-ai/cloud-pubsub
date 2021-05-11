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
import qualified Data.Time.Clock.POSIX         as POSIXTime

data ConsumerConfig = ConsumerConfig
  { pollBatchSize   :: Int
  , pollingInterval :: NominalDiffTime
  }

toDouble :: Real a => a -> Double
toDouble = realToFrac

-- The epochMicros function isn't monotonic
epochMicros :: MonadIO m => m Int
epochMicros = round . (* 1000000) . toDouble <$> liftIO POSIXTime.getPOSIXTime

runEvery :: MonadIO m => NominalDiffTime -> m () -> m ()
runEvery interval action = forever $ do
  start <- epochMicros
  action
  end <- epochMicros
  let elapsed  = end - start
      timeTill = round (1000000 * toDouble interval) - elapsed
  liftIO $ Concurrent.threadDelay timeTill

-- Still WIP
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
