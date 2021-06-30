module Cloud.PubSub.Snapshot.Types where

import qualified Cloud.PubSub.Core.Json        as Json
import           Cloud.PubSub.Core.Types        ( ProjectId(..)
                                                , UpdateMask
                                                )
import           Cloud.PubSub.Http.Types        ( PageToken )
import           Cloud.PubSub.Subscription.Types
                                                ( QualifiedSubName )
import qualified Data.Aeson                    as Aeson
import           Data.HashMap.Strict            ( HashMap )
import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           GHC.Generics                   ( Generic )

newtype SnapshotName  = SnapshotName
  { unwrapSnapshotName :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (IsString, Aeson.ToJSON, Aeson.FromJSON)

newtype QualifiedSnapshotName  = QualifiedSnapshotName
  { unwrapQualifiedSnapshotName :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (IsString, Aeson.ToJSON, Aeson.FromJSON)

qualifySnapshotName :: ProjectId -> SnapshotName -> QualifiedSnapshotName
qualifySnapshotName (ProjectId pId) (SnapshotName s) = do
  QualifiedSnapshotName $ "projects/" <> pId <> "/snapshots/" <> s

data SnapshotAlreadyExists = SnapshotAlreadyExists
  deriving stock (Show, Eq)

data NewSnapshot = NewSnapshot
  { subscription :: QualifiedSubName
  , labels       :: Maybe (HashMap Text Text)
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON NewSnapshot where
  toJSON = Aeson.genericToJSON Json.options

instance Aeson.FromJSON NewSnapshot where
  parseJSON = Aeson.genericParseJSON Json.options

minimalNewSnapshot :: QualifiedSubName -> NewSnapshot
minimalNewSnapshot subName =
  NewSnapshot { subscription = subName, labels = Nothing }

data Snapshot = Snapshot
  { name       :: QualifiedSnapshotName
  , topic      :: QualifiedSubName
  , expireTime :: UTCTime
  , labels     :: Maybe (HashMap Text Text)
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON Snapshot where
  toJSON = Aeson.genericToJSON Json.options

instance Aeson.FromJSON Snapshot where
  parseJSON = Aeson.genericParseJSON Json.options

data SnapshotListResponse = SnapshotListResponse
  { snapshots     :: [Snapshot]
  , nextPageToken :: Maybe PageToken
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON SnapshotListResponse where
  toJSON = Aeson.genericToJSON Json.options

instance Aeson.FromJSON SnapshotListResponse where
  parseJSON = Aeson.genericParseJSON Json.options

data SnapshotPatch = SnapshotPatch
  { snapshot   :: Snapshot
  , updateMask :: UpdateMask
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON SnapshotPatch where
  toJSON = Aeson.genericToJSON Json.options

instance Aeson.FromJSON SnapshotPatch where
  parseJSON = Aeson.genericParseJSON Json.options
