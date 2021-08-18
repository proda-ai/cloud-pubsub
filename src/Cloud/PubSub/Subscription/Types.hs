module Cloud.PubSub.Subscription.Types where

import qualified Cloud.PubSub.Core.Json        as Json
import           Cloud.PubSub.Core.Types        ( Base64DataString
                                                , MessageId
                                                , ProjectId(..)
                                                , QualifiedTopicName
                                                , TopicName
                                                , UpdateMask
                                                )
import           Cloud.PubSub.Http.Types        ( PageToken )
import qualified Data.Aeson                    as Aeson
import           Data.Aeson                     ( (.=) )
import           Data.HashMap.Strict            ( HashMap )
import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           GHC.Generics                   ( Generic )

newtype SubName = SubName
  { unwrapSubName :: Text
  }
  deriving stock (Show, Eq)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON, IsString)

newtype QualifiedSubName  = QualifiedSubName
  { unwrapQualifiedSubName :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (IsString, Aeson.ToJSON, Aeson.FromJSON)

qualifySubName :: ProjectId -> SubName -> QualifiedSubName
qualifySubName (ProjectId pId) (SubName s) = do
  QualifiedSubName $ "projects/" <> pId <> "/subscriptions/" <> s

data SeekTarget = SnapshotTarget Text
                | TimeTarget UTCTime
                  deriving stock (Show, Eq)

instance Aeson.ToJSON SeekTarget where
  toJSON target = case target of
    SnapshotTarget s -> Aeson.object ["snapshot" .= s]
    TimeTarget     t -> Aeson.object ["time" .= t]

newtype AckId = AckId
  { unwrapAckId :: Text
  }
  deriving stock (Show, Eq)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

data PubsubMessage = PubsubMessage
  { pmData        :: Base64DataString
  , pmMessageId   :: MessageId
  , pmAttributes  :: Maybe (HashMap Text Text)
  , pmPublishTime :: UTCTime
  , pmOrderingKey :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.FromJSON PubsubMessage where
  parseJSON = Aeson.genericParseJSON $ Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Json.lowercaseFirstChar . drop 2
    , Aeson.omitNothingFields  = True
    }

data ReceivedMessage = ReceivedMessage
  { ackId           :: AckId
  , message         :: PubsubMessage
  , deliveryAttempt :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.FromJSON ReceivedMessage where
  parseJSON = Aeson.genericParseJSON Json.options

newtype PullResponse = PullResponse
  { receivedMessages :: [ReceivedMessage]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Aeson.FromJSON

newtype PullRequest = PullRequest
  { maxMessages :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Aeson.ToJSON

newtype AcknowledgeRequest  = AcknowledgeRequest
  { ackIds :: [AckId]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Aeson.ToJSON

newtype ModifyPushConfigRequest = ModifyPushConfigRequest
  { pushConfig :: PushConfig
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Aeson.ToJSON

data ModifyAckDeadlineRequest = ModifyAckDeadlineRequest
  { ackIds             :: [AckId]
  , ackDeadlineSeconds :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Aeson.ToJSON

newtype ExpirationPolicy = ExpirationPolicy
  { ttl :: Maybe Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data OidcToken = OidcToken
  { serviceAccountEmail :: Text
  , audience            :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data PushConfig = PushConfig
  { pushEndpoint :: Maybe Text
  , attributes   :: Maybe (HashMap Text Text)
  , oidcToken    :: Maybe OidcToken
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data DeadLetterPolicy = DeadLetterPolicy
  { deadLetterTopic     :: TopicName
  , maxDeliveryAttempts :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data RetryPolicy = RetryPolicy
  { minimumBackoff :: Text
  , maximumBackoff :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data SubscriptionAlreadyExists = SubscriptionAlreadyExists
  deriving stock (Show, Eq)

data Subscription = Subscription
  { name                     :: QualifiedSubName
  , topic                    :: QualifiedTopicName
  , pushConfig               :: Maybe PushConfig
  , ackDeadlineSeconds       :: Maybe Int
  , retainAckedMessages      :: Maybe Bool
  , messageRetentionDuration :: Maybe Text
  , labels                   :: Maybe (HashMap Text Text)
  , enableMessageOrdering    :: Maybe Bool
  , expirationPolicy         :: Maybe ExpirationPolicy
  , filter                   :: Maybe Text
  , deadLetterPolicy         :: Maybe DeadLetterPolicy
  , retryPolicy              :: Maybe RetryPolicy
  , detached                 :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON Subscription where
  toJSON = Aeson.genericToJSON Json.options

instance Aeson.FromJSON Subscription where
  parseJSON = Aeson.genericParseJSON Json.options

data NewSubscription = NewSubscription
  { topic                    :: QualifiedTopicName
  , pushConfig               :: Maybe PushConfig
  , ackDeadlineSeconds       :: Maybe Int
  , retainAckedMessages      :: Maybe Bool
  , messageRetentionDuration :: Maybe Text
  , labels                   :: Maybe (HashMap Text Text)
  , enableMessageOrdering    :: Maybe Bool
  , expirationPolicy         :: Maybe ExpirationPolicy
  , filter                   :: Maybe Text
  , deadLetterPolicy         :: Maybe DeadLetterPolicy
  , retryPolicy              :: Maybe RetryPolicy
  , detached                 :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON NewSubscription where
  toJSON = Aeson.genericToJSON Json.options

instance Aeson.FromJSON NewSubscription where
  parseJSON = Aeson.genericParseJSON Json.options

minimalNewSubscription :: QualifiedTopicName -> NewSubscription
minimalNewSubscription topicName = NewSubscription
  { topic                    = topicName
  , pushConfig               = Nothing
  , ackDeadlineSeconds       = Nothing
  , retainAckedMessages      = Nothing
  , messageRetentionDuration = Nothing
  , labels                   = Nothing
  , enableMessageOrdering    = Nothing
  , expirationPolicy         = Nothing
  , filter                   = Nothing
  , deadLetterPolicy         = Nothing
  , retryPolicy              = Nothing
  , detached                 = Nothing
  }

data SubscriptionListResponse = SubscriptionListResponse
  { subscriptions :: [Subscription]
  , nextPageToken :: Maybe PageToken
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON SubscriptionListResponse where
  toJSON = Aeson.genericToJSON Json.options

instance Aeson.FromJSON SubscriptionListResponse where
  parseJSON = Aeson.genericParseJSON Json.options

data SubscriptionPatch = SubscriptionPatch
  { subscription :: Subscription
  , updateMask   :: UpdateMask
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON SubscriptionPatch where
  toJSON = Aeson.genericToJSON Json.options

instance Aeson.FromJSON SubscriptionPatch where
  parseJSON = Aeson.genericParseJSON Json.options
