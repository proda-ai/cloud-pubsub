module Cloud.PubSub.Publisher.Types
  ( BatchCapacity(..)
  , BatchId
  , HasPublisherResources(..)
  , PendingMessageBatch(..)
  , PublishResult(..)
  , PublisherConfig(..)
  , PublisherImpl(..)
  , PublisherResources(..)
  , MessageBatchSizeExceeded(..)
  , SentStatus(..)
  , genBatchId
  , pendingMessageBatchSize
  , publishResultCtx
  ) where


import           Cloud.PubSub.Core.Types        ( Message
                                                , MessageId
                                                , TopicName
                                                )
import           Control.Concurrent             ( MVar )
import           Control.Concurrent.STM.TQueue  ( TQueue )
import qualified Control.Immortal              as Immortal
import           Control.Monad.Catch            ( Exception
                                                , SomeException
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.Aeson                    as Aeson
import           Data.Aeson                     ( (.=) )
import           Data.Time                      ( NominalDiffTime )

import           Data.UUID                      ( UUID )
import qualified Data.UUID.V4                  as UUID
import           GHC.Generics                   ( Generic )

newtype BatchId =
  BatchId { unwrapBatchId :: UUID }
  deriving stock (Show, Eq)
  deriving newtype(Aeson.ToJSON)

genBatchId :: MonadIO m => m BatchId
genBatchId = BatchId <$> liftIO UUID.nextRandom

data BatchCapacity = Empty | HasRoom | Full
  deriving stock (Show, Eq, Generic)
  deriving anyclass(Aeson.ToJSON)

data SentStatus = Sent [MessageId]
                | Failed SomeException

data PublisherResources = PublisherResources
  { pendingMessageBatchQueue :: TQueue PendingMessageBatch
  , publisherConfig          :: PublisherConfig
  , worker                   :: Immortal.Thread
  }

class HasPublisherResources m where
  askPublisherResources :: m PublisherResources

data PublisherConfig = PublisherConfig
  { maxQueueMessageSize :: Int
  , maxBatchSize        :: Int
  , maxBatchDelay       :: NominalDiffTime
  }

data PendingMessageBatch = PendingMessageBatch
  { batchId  :: BatchId
  , topic    :: TopicName
  , status   :: MVar SentStatus
  , messages :: [Message]
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
