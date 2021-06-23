module Cloud.PubSub.Publisher.Types
  ( BatchId
  , HasPublisherResources(..)
  , PendingMessageBatch(..)
  , PublishResult(..)
  , PublisherConfig(..)
  , PublisherImpl(..)
  , PublisherResources(..)
  , MessageBatchSizeExceeded(..)
  , SentStatus(..)
  , TopicBatch(..)
  , TopicBatches
  , addToTopicBatch
  , newTopicBatch
  , genBatchId
  , pendingMessageBatchSize
  , publishResultCtx
  ) where


import           Cloud.PubSub.Core.Types        ( Message
                                                , MessageId
                                                , TopicName
                                                )
import           Control.Concurrent             ( MVar )
import           Control.Concurrent.STM.TVar    ( TVar )
import qualified Control.Immortal              as Immortal
import           Control.Monad.Catch            ( Exception
                                                , SomeException
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.Aeson                    as Aeson
import           Data.Aeson                     ( (.=) )
import           Data.Time                      ( NominalDiffTime
                                                , UTCTime
                                                )

import           Data.DList                     ( DList )
import qualified Data.DList                    as DList
import           Data.Map                       ( Map )
import           Data.UUID                      ( UUID )
import qualified Data.UUID.V4                  as UUID

newtype BatchId =
  BatchId { unwrapBatchId :: UUID }
  deriving stock (Show, Eq)
  deriving newtype(Aeson.ToJSON)

genBatchId :: MonadIO m => m BatchId
genBatchId = BatchId <$> liftIO UUID.nextRandom

data SentStatus = Sent [MessageId]
                | Failed SomeException

data TopicBatch = TopicBatch
  { tbTopic       :: TopicName
  , tbOldestBatch :: UTCTime
  , tbTotal       :: Int
  , tbBatches     :: DList PendingMessageBatch
  }

newTopicBatch :: PendingMessageBatch -> TopicBatch
newTopicBatch pmb = TopicBatch { tbTopic       = topic pmb
                               , tbOldestBatch = enqueueTime pmb
                               , tbTotal       = pendingMessageBatchSize pmb
                               , tbBatches     = DList.fromList [pmb]
                               }

addToTopicBatch :: PendingMessageBatch -> TopicBatch -> TopicBatch
addToTopicBatch pmb tb = TopicBatch
  { tbTopic       = tbTopic tb
  , tbOldestBatch = min (enqueueTime pmb) (tbOldestBatch tb)
  , tbTotal       = tbTotal tb + pendingMessageBatchSize pmb
  , tbBatches     = DList.snoc (tbBatches tb) pmb
  }

type TopicBatches = Map TopicName TopicBatch

data PublisherResources = PublisherResources
  { pendingTopicBatches :: TVar TopicBatches
  , publisherConfig     :: PublisherConfig
  , worker              :: Immortal.Thread
  }

class HasPublisherResources m where
  askPublisherResources :: m PublisherResources

data PublisherConfig = PublisherConfig
  { maxBatchSize  :: Int
  , maxBatchDelay :: NominalDiffTime
  }

data PendingMessageBatch = PendingMessageBatch
  { batchId     :: BatchId
  , enqueueTime :: UTCTime
  , topic       :: TopicName
  , status      :: MVar SentStatus
  , messages    :: [Message]
  }

pendingMessageBatchSize :: PendingMessageBatch -> Int
pendingMessageBatchSize = length . messages

newtype PublisherImpl = PublisherImpl {
  publish :: TopicName -> [Message] -> IO [MessageId]
}

data PublishResult = PublishResult
  { prTopic   :: TopicName
  , prBatchId :: BatchId
  , prStatus  :: MVar SentStatus
  }

publishResultCtx :: PublishResult -> Aeson.Value
publishResultCtx pr =
  Aeson.object ["topic" .= prTopic pr, "batchId" .= prBatchId pr]

data MessageBatchSizeExceeded = MessageBatchSizeExceeded
  { messageCount :: Int
  , maxBatch     :: Int
  }
  deriving Show

instance Exception MessageBatchSizeExceeded
