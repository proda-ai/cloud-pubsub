module Cloud.PubSub.Subscription
  ( acknowledge
  , create
  , createWithDefaults
  , delete
  , detach
  , get
  , modifyAckDeadline
  , modifyPushConfig
  , patch
  , pull
  , seek
  , list
  ) where

import qualified Cloud.PubSub.Core.Types       as Core
import           Cloud.PubSub.Core.Types        ( TopicName )
import qualified Cloud.PubSub.Http.Types       as HttpT
import qualified Cloud.PubSub.HttpClient       as HttpClient
import qualified Cloud.PubSub.Subscription.Types
                                               as SubscriptionT
import           Control.Monad.Catch            ( throwM )
import qualified Data.Text                     as Text

getSubStr
  :: (HttpT.HasGoogleProjectId m, Monad m) => SubscriptionT.SubName -> m String
getSubStr (SubscriptionT.SubName s) = do
  projectId <- HttpT.askProjectId
  let subPath =
        "/projects/" <> Core.unwrapProjectId projectId <> "/subscriptions/" <> s
  return $ "/v1" <> Text.unpack subPath

getSubPath
  :: (HttpT.HasGoogleProjectId m, Monad m)
  => SubscriptionT.SubName
  -> m HttpT.PathQueryParams
getSubPath = fmap HttpT.simplePath . getSubStr

getSubOpPath
  :: (HttpT.HasGoogleProjectId m, Monad m)
  => SubscriptionT.SubName
  -> String
  -> m HttpT.PathQueryParams
getSubOpPath subName op = HttpT.simplePath . flip (++) op <$> getSubStr subName

create
  :: HttpT.PubSubHttpClientM m
  => SubscriptionT.SubName
  -> SubscriptionT.NewSubscription
  -> m
       ( Either
           SubscriptionT.SubscriptionAlreadyExists
           SubscriptionT.Subscription
       )
create subName sub = do
  path <- getSubPath subName
  HttpClient.authedJsonPutRequest path sub >>= \case
    Right r                         -> return $ Right r
    Left  e@(HttpT.ResponseError m) -> if HttpT.isAlreadyExistsError m
      then return $ Left SubscriptionT.SubscriptionAlreadyExists
      else throwM e
    Left e -> throwM e

createWithDefaults
  :: HttpT.PubSubHttpClientM m
  => SubscriptionT.SubName
  -> TopicName
  -> m
       ( Either
           SubscriptionT.SubscriptionAlreadyExists
           SubscriptionT.Subscription
       )
createWithDefaults subName topicName = do
  projectId <- HttpT.askProjectId
  let qualifiedTopic = Core.qualifyTopicName projectId topicName
  let sub            = SubscriptionT.minimalNewSubscription qualifiedTopic
  create subName sub

get
  :: HttpT.PubSubHttpClientM m
  => SubscriptionT.SubName
  -> m SubscriptionT.Subscription
get subName = do
  path <- getSubPath subName
  HttpClient.authedJsonGetRequest path

delete :: HttpT.PubSubHttpClientM m => SubscriptionT.SubName -> m ()
delete subName = do
  path <- getSubPath subName
  HttpClient.authedDeleteRequest path

patch
  :: HttpT.PubSubHttpClientM m
  => SubscriptionT.SubName
  -> SubscriptionT.SubscriptionPatch
  -> m SubscriptionT.Subscription
patch subName subPatch = do
  path <- getSubPath subName
  HttpClient.authedJsonPatchRequest path subPatch >>= either throwM return

pull
  :: HttpT.PubSubHttpClientM m
  => SubscriptionT.SubName
  -> Int
  -> m [SubscriptionT.ReceivedMessage]
pull subName batchSize = do
  path <- getSubOpPath subName ":pull"
  let body = SubscriptionT.PullRequest batchSize
  SubscriptionT.receivedMessages
    <$> (HttpClient.authedJsonPostRequest path body >>= either throwM return)

acknowledge
  :: HttpT.PubSubHttpClientM m
  => SubscriptionT.SubName
  -> [SubscriptionT.AckId]
  -> m ()
acknowledge subName ackIds = do
  path <- getSubOpPath subName ":acknowledge"
  let body = SubscriptionT.AcknowledgeRequest ackIds
  HttpClient.authedNoContentPostRequest path body

modifyAckDeadline
  :: HttpT.PubSubHttpClientM m
  => SubscriptionT.SubName
  -> [SubscriptionT.AckId]
  -> Int
  -> m ()
modifyAckDeadline subName ackIds deadlineSecs = do
  path <- getSubOpPath subName ":modifyAckDeadline"
  let body = SubscriptionT.ModifyAckDeadlineRequest ackIds deadlineSecs
  HttpClient.authedNoContentPostRequest path body

modifyPushConfig
  :: HttpT.PubSubHttpClientM m
  => SubscriptionT.SubName
  -> SubscriptionT.PushConfig
  -> m ()
modifyPushConfig subName pushConfig = do
  path <- getSubOpPath subName ":modifyPushConfig"
  let body = SubscriptionT.ModifyPushConfigRequest pushConfig
  HttpClient.authedNoContentPostRequest path body

detach :: HttpT.PubSubHttpClientM m => SubscriptionT.SubName -> m ()
detach subName = do
  path <- getSubOpPath subName ":detach"
  HttpClient.authedNoBodyPostRequest path

seek
  :: HttpT.PubSubHttpClientM m
  => SubscriptionT.SubName
  -> SubscriptionT.SeekTarget
  -> m ()
seek subName target = do
  path <- getSubOpPath subName ":seek"
  HttpClient.authedNoContentPostRequest path target

list
  :: HttpT.PubSubHttpClientM m
  => Maybe HttpT.PageQuery
  -> m SubscriptionT.SubscriptionListResponse
list pageQuery = do
  projectId <- Core.unwrapProjectId <$> HttpT.askProjectId
  let path = "/v1/projects/" <> Text.unpack projectId <> "/subscriptions"
      pathQueryParams =
        HttpT.PathQueryParams path $ fmap HttpT.pageQueryParams pageQuery
  HttpClient.authedJsonGetRequest pathQueryParams
