module Cloud.PubSub.Topic
  ( create
  , createWithDefaults
  , delete
  , get
  , list
  , patch
  , publish
  ) where

import qualified Cloud.PubSub.Core.Types       as Core
import           Cloud.PubSub.Core.Types        ( MessageId
                                                , TopicName(..)
                                                )
import qualified Cloud.PubSub.Http.Types       as HttpT
import qualified Cloud.PubSub.HttpClient       as HttpClient
import qualified Cloud.PubSub.Topic.Types      as TopicT
import           Control.Monad.Catch            ( throwM )
import qualified Data.Text                     as Text

getTopicStr :: (HttpT.HasGoogleProjectId m, Monad m) => TopicName -> m String
getTopicStr (TopicName t) = do
  projectId <- Core.unwrapProjectId <$> HttpT.askProjectId
  let topicPath = "/projects/" <> projectId <> "/topics/" <> t
  return $ Text.unpack $ "/v1" <> topicPath

getTopicPath
  :: (HttpT.HasGoogleProjectId m, Monad m)
  => TopicName
  -> m HttpT.PathQueryParams
getTopicPath = fmap HttpT.simplePath . getTopicStr

getTopicOpPath
  :: (HttpT.HasGoogleProjectId m, Monad m)
  => TopicName
  -> String
  -> m HttpT.PathQueryParams
getTopicOpPath topicName op =
  HttpT.simplePath . (++ (':' : op)) <$> getTopicStr topicName

create
  :: HttpT.PubSubHttpClientM m
  => TopicName
  -> TopicT.NewTopic
  -> m (Either TopicT.TopicAlreadyExists TopicT.Topic)
create topicName newTopic = do
  path <- getTopicPath topicName
  HttpClient.authedJsonPutRequest path newTopic >>= \case
    Right r                         -> return $ Right r
    Left  e@(HttpT.ResponseError m) -> if HttpT.isAlreadyExistsError m
      then return $ Left TopicT.TopicAlreadyExists
      else throwM e
    Left e -> throwM e

createWithDefaults
  :: HttpT.PubSubHttpClientM m
  => TopicName
  -> m (Either TopicT.TopicAlreadyExists TopicT.Topic)
createWithDefaults topicName = create topicName TopicT.minimalNewTopic

get :: HttpT.PubSubHttpClientM m => TopicName -> m TopicT.Topic
get topicName = do
  path <- getTopicPath topicName
  HttpClient.authedJsonGetRequest path

list
  :: HttpT.PubSubHttpClientM m
  => Maybe HttpT.PageQuery
  -> m TopicT.TopicListResponse
list pageQuery = do
  projectId <- Core.unwrapProjectId <$> HttpT.askProjectId
  let path = "/v1/projects/" <> Text.unpack projectId <> "/topics"
      pathQueryParams =
        HttpT.PathQueryParams path $ fmap HttpT.pageQueryParams pageQuery
  HttpClient.authedJsonGetRequest pathQueryParams

patch
  :: HttpT.PubSubHttpClientM m
  => TopicName
  -> TopicT.TopicPatch
  -> m TopicT.Topic
patch topicName topicPatch = do
  path <- getTopicPath topicName
  HttpClient.authedJsonPatchRequest path topicPatch >>= either throwM return

delete :: HttpT.PubSubHttpClientM m => TopicName -> m ()
delete topicName = do
  path <- getTopicPath topicName
  HttpClient.authedDeleteRequest path

publish
  :: HttpT.PubSubHttpClientM m
  => TopicName
  -> [TopicT.PublishPubsubMessage]
  -> m [MessageId]
publish topicName messages = do
  path <- getTopicOpPath topicName "publish"
  TopicT.messageIds
    <$> (HttpClient.authedJsonPostRequest path body >>= either throwM return)
  where body = TopicT.PubsubMessageBatch messages
