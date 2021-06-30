module Cloud.PubSub.Core.Types
  ( Base64DataString(..)
  , Message(..)
  , MessageId(..)
  , ProjectId(..)
  , TopicName(..)
  , QualifiedTopicName
  , UpdateMask(..)
  , qualifyTopicName
  ) where

import qualified Data.Aeson                    as Aeson
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Base64        as Base64
import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as TE
import           GHC.Generics                   ( Generic )

data Message = Message
  { key   :: Maybe Text
  , value :: ByteString
  }
  deriving (Show, Eq)

newtype TopicName  = TopicName
  { unwrapTopicName :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (IsString, Aeson.ToJSON, Aeson.FromJSON, Ord)

newtype ProjectId = ProjectId {unwrapProjectId :: Text}
  deriving stock (Show)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

newtype QualifiedTopicName  = QualifiedTopicName
  { unwrapQualifiedTopicName :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (IsString, Aeson.ToJSON, Aeson.FromJSON)

qualifyTopicName :: ProjectId -> TopicName -> QualifiedTopicName
qualifyTopicName (ProjectId pId) (TopicName t) = do
  QualifiedTopicName $ "projects/" <> pId <> "/topics/" <> t

newtype Base64DataString = Base64DataString { unwrapBase64DataString :: ByteString }
  deriving stock (Show, Eq)
  deriving newtype (IsString)

instance Aeson.ToJSON Base64DataString where
  toJSON =
    Aeson.toJSON . TE.decodeUtf8 . Base64.encode . unwrapBase64DataString

instance Aeson.FromJSON  Base64DataString where
  parseJSON v = do
    base64bytestring <- TE.encodeUtf8 <$> Aeson.parseJSON v
    case Base64.decode base64bytestring of
      Left  e  -> fail e
      Right bs -> return $ Base64DataString bs

newtype MessageId = MessageId
  { unwrapMessageId :: Text
  }
  deriving stock (Show, Eq)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

newtype UpdateMask = UpdateMask
  { unwrapUpdateMask ::  Text
  }
  deriving stock (Show, Eq)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)
