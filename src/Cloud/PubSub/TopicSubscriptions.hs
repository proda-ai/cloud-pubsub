module Cloud.PubSub.TopicSubscriptions
  ( TopicSubscriptionListResponse(..)
  , list
  ) where

import qualified Cloud.PubSub.Core.Json        as Json
import qualified Cloud.PubSub.Core.Types       as Core
import           Cloud.PubSub.Core.Types        ( TopicName(..) )
import qualified Cloud.PubSub.Http.Types       as HttpT
import           Cloud.PubSub.Http.Types        ( PageToken )
import qualified Cloud.PubSub.HttpClient       as HttpClient
import qualified Cloud.PubSub.Subscription.Types
                                               as SubscriptionT
import qualified Data.Aeson                    as Aeson
import qualified Data.Text                     as Text
import           GHC.Generics                   ( Generic )

data TopicSubscriptionListResponse = TopicSubscriptionListResponse
  { subscriptions :: [SubscriptionT.QualifiedSubName]
  , nextPageToken :: Maybe PageToken
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON TopicSubscriptionListResponse where
  toJSON = Aeson.genericToJSON Json.options

instance Aeson.FromJSON TopicSubscriptionListResponse where
  parseJSON = Aeson.genericParseJSON Json.options

list
  :: HttpT.PubSubHttpClientM m
  => TopicName
  -> Maybe HttpT.PageQuery
  -> m TopicSubscriptionListResponse
list (TopicName t) pageQuery = do
  projectId <- Core.unwrapProjectId <$> HttpT.askProjectId
  let path =
        Text.unpack
          $  "/v1/projects/"
          <> projectId
          <> "/topics/"
          <> t
          <> "/subscriptions"
      pathQueryParams =
        HttpT.PathQueryParams path $ fmap HttpT.pageQueryParams pageQuery
  HttpClient.authedJsonGetRequest pathQueryParams
