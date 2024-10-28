module Cloud.PubSub.Topic.Types where

import qualified Cloud.PubSub.Core.Json        as Json
import           Cloud.PubSub.Core.Types        ( Base64DataString
                                                , MessageId
                                                , QualifiedTopicName
                                                , UpdateMask
                                                )
import           Cloud.PubSub.Http.Types        ( PageToken )
import qualified Data.Aeson                    as Aeson
import           Data.HashMap.Strict            ( HashMap )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

data PublishPubsubMessage = PublishPubsubMessage
  { ppmOrderingKey :: Maybe Text
  , ppmData        :: Base64DataString
  , ppmAttributes  :: Maybe (HashMap Text Text)
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON PublishPubsubMessage where
  toJSON = Aeson.genericToJSON $ Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Json.lowercaseFirstChar . drop 3
    }

newtype PubsubMessageBatch = PubsubMessageBatch
  { messages :: [PublishPubsubMessage]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Aeson.ToJSON

newtype PublishResponse = PublishResponse
  { messageIds :: [MessageId]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Aeson.FromJSON

newtype Region = Region
  { unwrapRegion :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

newtype MessageStoragePolicy = MessageStoragePolicy
  { allowedPersistenceRegions :: [Region]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data Encoding = EncodingUnspecified
              | Json
              | Binary
                deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON Encoding where
  toJSON = Aeson.genericToJSON Json.options

instance Aeson.FromJSON Encoding where
  parseJSON = Aeson.genericParseJSON Json.options

data SchemaSettings = SchemaSettings
  { schema   :: Text
  , encoding :: Encoding
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data NewTopic = NewTopic
  { labels               :: Maybe (HashMap Text Text)
  , messageStoragePolicy :: Maybe MessageStoragePolicy
  , kmsKeyName           :: Maybe Text
  , schemaSettings       :: Maybe SchemaSettings
  , satisfiesPzs         :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON NewTopic where
  toJSON = Aeson.genericToJSON Json.options

instance Aeson.FromJSON NewTopic where
  parseJSON = Aeson.genericParseJSON Json.options

data TopicAlreadyExists = TopicAlreadyExists
  deriving stock (Show, Eq)

data Topic = Topic
  { name                 :: QualifiedTopicName
  , labels               :: Maybe (HashMap Text Text)
  , messageStoragePolicy :: Maybe MessageStoragePolicy
  , kmsKeyName           :: Maybe Text
  , schemaSettings       :: Maybe SchemaSettings
  , satisfiesPzs         :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON Topic where
  toJSON = Aeson.genericToJSON Json.options

instance Aeson.FromJSON Topic where
  parseJSON = Aeson.genericParseJSON Json.options

minimalNewTopic :: NewTopic
minimalNewTopic = NewTopic { labels               = Nothing
                           , messageStoragePolicy = Nothing
                           , kmsKeyName           = Nothing
                           , schemaSettings       = Nothing
                           , satisfiesPzs         = Nothing
                           }

data TopicListResponse = TopicListResponse
  { topics        :: [Topic]
  , nextPageToken :: Maybe PageToken
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON TopicListResponse where
  toJSON = Aeson.genericToJSON Json.options

instance Aeson.FromJSON TopicListResponse where
  parseJSON = Aeson.genericParseJSON Json.options

data TopicPatch = TopicPatch
  { topic      :: Topic
  , updateMask :: UpdateMask
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON TopicPatch where
  toJSON = Aeson.genericToJSON Json.options

instance Aeson.FromJSON TopicPatch where
  parseJSON = Aeson.genericParseJSON Json.options
