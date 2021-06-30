module Cloud.PubSub.Schema.Types
  ( SchemaName(..)
  , QualifiedSchemaName
  , Schema(..)
  , SchemaType(..)
  , SchemaDefinition(..)
  , SchemaView(..)
  , CreatedSchema(..)
  , SchemaListResponse(..)
  , SchemaAlreadyExists(..)
  , SchemaValidationError(..)
  , ValidateSchema(..)
  , ValidateMessage(..)
  , qualifySchemaName
  ) where

import qualified Cloud.PubSub.Core.Json        as Json
import           Cloud.PubSub.Core.Types        ( Base64DataString
                                                , ProjectId(..)
                                                )
import           Cloud.PubSub.Http.Types        ( PageToken )
import           Cloud.PubSub.Topic.Types       ( Encoding )
import qualified Data.Aeson                    as Aeson
import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype SchemaName  = SchemaName
  { unwrapSchemaName :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (IsString, Aeson.ToJSON, Aeson.FromJSON)

newtype QualifiedSchemaName  = QualifiedSchemaName
  { unwrapQualifiedSchemaName :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (IsString, Aeson.ToJSON, Aeson.FromJSON)

qualifySchemaName :: ProjectId -> SchemaName -> QualifiedSchemaName
qualifySchemaName (ProjectId pId) (SchemaName s) = do
  QualifiedSchemaName $ "projects/" <> pId <> "/schemas/" <> s

newtype SchemaDefinition = SchemaDefinition
  { unwrapSchemaDefinition :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (IsString, Aeson.ToJSON, Aeson.FromJSON)

data SchemaView  = SchemaViewUnspecified
                 | Basic
                 | Full
                   deriving (Show, Eq)

jsonOptionsWithPrefix :: Int -> Aeson.Options
jsonOptionsWithPrefix n =
  Json.options { Aeson.fieldLabelModifier = Json.lowercaseFirstChar . drop n }

data CreatedSchema = CreatedSchema
  { csName :: QualifiedSchemaName
  , csType :: SchemaType
  }
  deriving stock (Show, Eq, Generic)

createdSchemaJsonOptions :: Aeson.Options
createdSchemaJsonOptions = jsonOptionsWithPrefix 2

instance Aeson.ToJSON CreatedSchema where
  toJSON = Aeson.genericToJSON createdSchemaJsonOptions

instance Aeson.FromJSON CreatedSchema where
  parseJSON = Aeson.genericParseJSON createdSchemaJsonOptions

data Schema = Schema
  { sName       :: QualifiedSchemaName
  , sType       :: SchemaType
  , sDefinition :: Maybe SchemaDefinition
  }
  deriving stock (Show, Eq, Generic)

schemaJsonOptions :: Aeson.Options
schemaJsonOptions = jsonOptionsWithPrefix 1

instance Aeson.ToJSON Schema where
  toJSON = Aeson.genericToJSON schemaJsonOptions

instance Aeson.FromJSON Schema where
  parseJSON = Aeson.genericParseJSON schemaJsonOptions

newtype ValidateSchema  = ValidateSchema
  { schema :: Schema
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data ValidateMessage = ValidateMessage
  { message  :: Base64DataString
  , encoding :: Encoding
  -- name and schema are mutually exclusive
  , name     :: Maybe QualifiedSchemaName
  , schema   :: Maybe Schema
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data SchemaType = TypeUnspecified
                | ProtocolBuffer
                | Avro
                  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON SchemaType where
  toJSON = Aeson.genericToJSON Json.options

instance Aeson.FromJSON SchemaType where
  parseJSON = Aeson.genericParseJSON Json.options

data SchemaListResponse = SchemaListResponse
  { schemas       :: [Schema]
  , nextPageToken :: Maybe PageToken
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON SchemaListResponse where
  toJSON = Aeson.genericToJSON Json.options

instance Aeson.FromJSON SchemaListResponse where
  parseJSON = Aeson.genericParseJSON Json.options

data SchemaAlreadyExists = SchemaAlreadyExists
  deriving stock (Show, Eq)

newtype SchemaValidationError = SchemaValidationError {
  unwrapSchemaValidationError :: Text
} deriving stock (Show, Eq)
