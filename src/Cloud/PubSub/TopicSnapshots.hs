module Cloud.PubSub.TopicSnapshots
  ( TopicSnapshotListResponse(..)
  , list
  ) where

import qualified Cloud.PubSub.Core.Json        as Json
import qualified Cloud.PubSub.Core.Types       as Core
import           Cloud.PubSub.Core.Types        ( TopicName(..) )
import qualified Cloud.PubSub.Http.Types       as HttpT
import           Cloud.PubSub.Http.Types        ( PageToken )
import qualified Cloud.PubSub.HttpClient       as HttpClient
import qualified Cloud.PubSub.Snapshot.Types   as SnapshotT
import qualified Data.Aeson                    as Aeson
import qualified Data.Text                     as Text
import           GHC.Generics                   ( Generic )

data TopicSnapshotListResponse = TopicSnapshotListResponse
  { snapshots     :: [SnapshotT.QualifiedSnapshotName]
  , nextPageToken :: Maybe PageToken
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON TopicSnapshotListResponse where
  toJSON = Aeson.genericToJSON Json.options

instance Aeson.FromJSON TopicSnapshotListResponse where
  parseJSON = Aeson.genericParseJSON Json.options

list
  :: HttpT.PubSubHttpClientM m
  => TopicName
  -> Maybe HttpT.PageQuery
  -> m TopicSnapshotListResponse
list (TopicName t) pageQuery = do
  projectId <- Core.unwrapProjectId <$> HttpT.askProjectId
  let path =
        Text.unpack
          $  "/v1/projects/"
          <> projectId
          <> "/topics/"
          <> t
          <> "/snapshots"
      pathQueryParams =
        HttpT.PathQueryParams path $ fmap HttpT.pageQueryParams pageQuery
  HttpClient.authedJsonGetRequest pathQueryParams
