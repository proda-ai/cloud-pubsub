{-# LANGUAGE ScopedTypeVariables #-}
module Cloud.PubSub.Schema
  ( create
  , delete
  , get
  , list
  , validate
  , validateMessage
  ) where

import qualified Cloud.PubSub.Core.Types       as Core
import qualified Cloud.PubSub.Http.Types       as HttpT
import qualified Cloud.PubSub.HttpClient       as HttpClient
import qualified Cloud.PubSub.Schema.Types     as SchemaT
import           Control.Monad.Catch            ( throwM )
import qualified Data.Aeson                    as Aeson
import           Data.Functor                   ( (<&>) )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as TE

getSchemaRootStr :: (HttpT.HasGoogleProjectId m, Monad m) => m String
getSchemaRootStr = do
  projectId <- Core.unwrapProjectId <$> HttpT.askProjectId
  return $ "/v1/projects/" <> Text.unpack projectId <> "/schemas"

getSchemaPath
  :: (HttpT.HasGoogleProjectId m, Monad m)
  => SchemaT.SchemaName
  -> m HttpT.PathQueryParams
getSchemaPath (SchemaT.SchemaName s) = do
  projectId <- Core.unwrapProjectId <$> HttpT.askProjectId
  let schemaPath = "/projects/" <> projectId <> "/schemas/" <> s
  return $ HttpT.simplePath $ "/v1" <> Text.unpack schemaPath

getSchemasOpPath
  :: (HttpT.HasGoogleProjectId m, Monad m) => String -> m HttpT.PathQueryParams
getSchemasOpPath op = do
  projectId <- Core.unwrapProjectId <$> HttpT.askProjectId
  return
    $  HttpT.simplePath
    $  "/v1/projects/"
    <> Text.unpack projectId
    <> "/schemas:"
    <> op

create
  :: HttpT.PubSubHttpClientM m
  => SchemaT.SchemaName
  -> SchemaT.SchemaType
  -> SchemaT.SchemaDefinition
  -> m (Either SchemaT.SchemaAlreadyExists SchemaT.CreatedSchema)
create schemaName schemaType definition = do
  projectId <- HttpT.askProjectId
  let qualifiedSchemaName = SchemaT.qualifySchemaName projectId schemaName
  let schema = SchemaT.Schema { sName       = qualifiedSchemaName
                              , sType       = schemaType
                              , sDefinition = Just definition
                              }
  let params = HttpT.QueryParams
        [ ( "schemaId"
          , Just $ TE.encodeUtf8 $ SchemaT.unwrapSchemaName schemaName
          )
        ]
  pathQueryParams <- (`HttpT.PathQueryParams` Just params) <$> getSchemaRootStr
  HttpClient.authedJsonPostRequest pathQueryParams schema >>= \case
    Right r -> return $ Right r
    Left e@(HttpT.ErrorResponseError _ m) -> if HttpT.isAlreadyExistsError m
      then return $ Left SchemaT.SchemaAlreadyExists
      else throwM e
    Left e -> throwM e

delete :: HttpT.PubSubHttpClientM m => SchemaT.SchemaName -> m ()
delete schemaName = do
  path <- getSchemaPath schemaName
  HttpClient.authedDeleteRequest path

schemaViewParams :: SchemaT.SchemaView -> HttpT.QueryParams
schemaViewParams schemaView =
  let view = case schemaView of
        SchemaT.SchemaViewUnspecified -> "SCHEMA_VIEW_UNSPECIFIED"
        SchemaT.Basic                 -> "BASIC"
        SchemaT.Full                  -> "FULL"
  in  HttpT.QueryParams [("view", Just view)]

get
  :: HttpT.PubSubHttpClientM m
  => SchemaT.SchemaName
  -> SchemaT.SchemaView
  -> m SchemaT.Schema
get schemaName schemaView = do
  path <-
    getSchemaPath schemaName
      <&> (\p -> p { HttpT.queryParams = Just $ schemaViewParams schemaView })
  HttpClient.authedJsonGetRequest path

list
  :: HttpT.PubSubHttpClientM m
  => SchemaT.SchemaView
  -> Maybe HttpT.PageQuery
  -> m SchemaT.SchemaListResponse
list schemaView pageQuery = do
  path <- getSchemaRootStr <&> (\p -> HttpT.PathQueryParams p (Just params))
  HttpClient.authedJsonGetRequest path
 where
  params =
    maybe mempty HttpT.pageQueryParams pageQuery <> schemaViewParams schemaView

emptyObject :: Aeson.Value
emptyObject = Aeson.object []

validate
  :: HttpT.PubSubHttpClientM m
  => SchemaT.Schema
  -> m (Maybe SchemaT.SchemaValidationError)
validate schema = do
  path <- getSchemasOpPath "validate"
  HttpClient.authedJsonPostRequest path body >>= \case
    Right v -> if v == emptyObject
      then return Nothing
      else error "unexpected validation response"
    Left e@(HttpT.ErrorResponseError _ m) -> if HttpT.isInvalidArgumentError m
      then return $ Just $ SchemaT.SchemaValidationError $ HttpT.message m
      else throwM e
    Left e -> throwM e
  where body = SchemaT.ValidateSchema schema

validateMessage
  :: HttpT.PubSubHttpClientM m
  => SchemaT.ValidateMessage
  -> m (Maybe SchemaT.SchemaValidationError)
validateMessage vm = do
  path <- getSchemasOpPath "validateMessage"
  HttpClient.authedJsonPostRequest path vm >>= \case
    Right v -> if v == emptyObject
      then return Nothing
      else error "unexpected validation response"
    Left e@(HttpT.ErrorResponseError _ m) -> if HttpT.isInvalidArgumentError m
      then return $ Just $ SchemaT.SchemaValidationError $ HttpT.message m
      else throwM e
    Left e -> throwM e
