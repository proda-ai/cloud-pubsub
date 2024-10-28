{-# LANGUAGE QuasiQuotes #-}

module Cloud.PubSub.SchemaSpec where

import           Cloud.PubSub.Core.Types        ( Base64DataString(..) )
import qualified Cloud.PubSub.Http.Types       as HttpT
import qualified Cloud.PubSub.Schema           as Schema
import qualified Cloud.PubSub.Schema.Types     as SchemaT
import           Cloud.PubSub.TestHelpers       ( TestEnv
                                                , mkTestPubSubEnv                                                
                                                , runTestIfNotEmulator
                                                )
import qualified Cloud.PubSub.Topic.Types      as TopicT
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.Aeson                    as Aeson
import           Data.Aeson.QQ                  ( aesonQQ )
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text.Encoding            as TE

import           Control.Monad.Catch            ( MonadMask
                                                , bracket
                                                )
import           Test.Hspec

jsonDataString :: Aeson.Value -> Base64DataString
jsonDataString = Base64DataString . LBS.toStrict . Aeson.encode

sampleAvroSchema :: SchemaT.SchemaDefinition
sampleAvroSchema = SchemaT.SchemaDefinition $ encodeText $ [aesonQQ| 
    {
      "type": "record",
      "name": "Sample",
      "fields" : [
        {"name": "value", "type": "long"}           
      ]
    }
  |]
  where encodeText = TE.decodeUtf8 . LBS.toStrict . Aeson.encode

withTestSchema
  :: (HttpT.PubSubHttpClientM m, MonadMask m)
  => SchemaT.SchemaName
  -> SchemaT.SchemaType
  -> SchemaT.SchemaDefinition
  -> (SchemaT.CreatedSchema -> m a)
  -> m a
withTestSchema schemaName schemaType schemaDef action = do
  bracket (Schema.delete schemaName >> createSchema)
          (const $ Schema.delete schemaName)
          action
 where
  createSchema = Schema.create schemaName schemaType schemaDef >>= \case
    Right cs -> return cs
    Left  e  -> error $ "unexpected existing schema " <> show e

schemaManagementTest :: TestEnv -> IO ()
schemaManagementTest = runTestIfNotEmulator $ do
  Schema.delete schemaName
  Right createdSchema <- Schema.create schemaName SchemaT.Avro sampleAvroSchema
  fetchedSchema       <- Schema.get schemaName SchemaT.Full
  Schema.delete schemaName
  let expected = SchemaT.Schema (SchemaT.csName createdSchema)
                                (SchemaT.csType createdSchema)
                                (Just sampleAvroSchema)
  liftIO $ fetchedSchema `shouldBe` expected
  where schemaName = "schema-management-test"

duplicateSchemaTest :: TestEnv -> IO ()
duplicateSchemaTest =
  runTestIfNotEmulator $ withTestSchema schemaName SchemaT.Avro sampleAvroSchema $ \_ -> do
    result <- Schema.create schemaName SchemaT.Avro sampleAvroSchema
    liftIO $ result `shouldBe` Left SchemaT.SchemaAlreadyExists
  where schemaName = "duplicate-schema-test"

schemaListTest :: TestEnv -> IO ()
schemaListTest =
  runTestIfNotEmulator
    $ withTestSchema schemaName SchemaT.Avro sampleAvroSchema
    $ \createdSchema -> do
        fetchedSchemas <- SchemaT.schemas <$> Schema.list SchemaT.Full Nothing
        let expected = SchemaT.Schema (SchemaT.csName createdSchema)
                                      (SchemaT.csType createdSchema)
                                      (Just sampleAvroSchema)
        liftIO $ fetchedSchemas `shouldContain` [expected]
  where schemaName = "schema-list-test"

schemaPaginiatedListTest :: TestEnv -> IO ()
schemaPaginiatedListTest =
  runTestIfNotEmulator
    $ withTestSchema schema1 SchemaT.Avro sampleAvroSchema
    $ const
    $ withTestSchema schema2 SchemaT.Avro sampleAvroSchema
    $ const
    $ do
        fetchedSchema1 <- Schema.list SchemaT.Full $ Just $ HttpT.PageQuery
          1
          Nothing
        let nextToken = SchemaT.nextPageToken fetchedSchema1
        fetchedSchema2 <- Schema.list SchemaT.Full $ Just $ HttpT.PageQuery
          1
          nextToken
        -- we only know that there are at least two schemas, given
        -- that there could be many others we only see if the params
        -- work
        liftIO $ do
          SchemaT.schemas fetchedSchema1 `shouldSatisfy` ((== 1) . length)
          SchemaT.schemas fetchedSchema2 `shouldSatisfy` ((== 1) . length)
 where
  schema1 = "schema-1-list-test"
  schema2 = "schema-2-list-test"

schemaValidateTest :: TestEnv -> IO ()
schemaValidateTest = runTestIfNotEmulator $ do
  let schema = SchemaT.Schema schemaName SchemaT.Avro (Just sampleAvroSchema)
  result <- Schema.validate schema
  liftIO $ result `shouldBe` Nothing
  where schemaName = "valid-schema-valdiate-test"

invalidSchemaValidateTest :: TestEnv -> IO ()
invalidSchemaValidateTest = runTestIfNotEmulator $ do
  let schema = SchemaT.Schema schemaName SchemaT.Avro (Just badSchema)
  result <- Schema.validate schema
  let expected = SchemaT.SchemaValidationError
        "AVRO schema definition is not valid: Tried to parse invalid JSON."
  liftIO $ result `shouldBe` Just expected
 where
  schemaName = "invalid-schema-valdiate-test"
  badSchema  = SchemaT.SchemaDefinition ""

validateMessageTest :: TestEnv -> IO ()
validateMessageTest =
  runTestIfNotEmulator
    $ withTestSchema schemaName SchemaT.Avro sampleAvroSchema
    $ \createdSchema -> do
        let validateMessageRequest = SchemaT.ValidateMessage
              { message  = jsonDataString [aesonQQ| { "value": 123 } |]
              , encoding = TopicT.Json
              , name     = Just $ SchemaT.csName createdSchema
              , schema   = Nothing
              }
        result <- Schema.validateMessage validateMessageRequest
        liftIO $ result `shouldBe` Nothing
  where schemaName = "valdiate-message-test"

validateInvalidMessageTest :: TestEnv -> IO ()
validateInvalidMessageTest =
  runTestIfNotEmulator
    $ withTestSchema schemaName SchemaT.Avro sampleAvroSchema
    $ \createdSchema -> do
        let validateMessageRequest = SchemaT.ValidateMessage
              { message  = jsonDataString [aesonQQ| { "value": "other" } |]
              , encoding = TopicT.Json
              , name     = Just $ SchemaT.csName createdSchema
              , schema   = Nothing
              }
        result <- Schema.validateMessage validateMessageRequest
        let
          expected =
            SchemaT.SchemaValidationError
              "Invalid data in message: JSON object with type string does not \
              \match schema which expected number_integer."
        liftIO $ result `shouldBe` Just expected
  where schemaName = "valdiate-invalid-message-test"

spec :: Spec
spec = parallel $ before mkTestPubSubEnv $ do
  describe "Schema Endpoints" $ do
    it "can create/get/delete a schema"               schemaManagementTest
    it "returns an error if the schema name is taken" duplicateSchemaTest
    it "can list schemas"                             schemaListTest
    it "can list schemas with pagination"             schemaPaginiatedListTest
    it "can validate a schema"                        schemaValidateTest
    it "returns an error when an invalid schema is validated"
       invalidSchemaValidateTest
    it "can validate a message" validateMessageTest
    it "returns an error when an invalid message is validated"
       validateInvalidMessageTest
