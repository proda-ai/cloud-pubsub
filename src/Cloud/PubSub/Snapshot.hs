module Cloud.PubSub.Snapshot
  ( create
  , createWithDefaults
  , delete
  , get
  , patch
  , list
  ) where

import qualified Cloud.PubSub.Core.Types       as Core
import qualified Cloud.PubSub.Http.Types       as HttpT
import qualified Cloud.PubSub.HttpClient       as HttpClient
import qualified Cloud.PubSub.Snapshot.Types   as SnapshotT
import qualified Cloud.PubSub.Subscription.Types
                                               as SubscriptionT
import           Control.Monad.Catch            ( throwM )
import qualified Data.Text                     as Text

getSnapshotStr
  :: (HttpT.HasGoogleProjectId m, Monad m) => SnapshotT.SnapshotName -> m String
getSnapshotStr (SnapshotT.SnapshotName s) = do
  projectId <- Core.unwrapProjectId <$> HttpT.askProjectId
  let snapshotPath = "/projects/" <> projectId <> "/snapshots/" <> s
  return $ "/v1" <> Text.unpack snapshotPath

getSnapshotPath
  :: (HttpT.HasGoogleProjectId m, Monad m)
  => SnapshotT.SnapshotName
  -> m HttpT.PathQueryParams
getSnapshotPath = fmap HttpT.simplePath . getSnapshotStr

create
  :: HttpT.PubSubHttpClientM m
  => SnapshotT.SnapshotName
  -> SnapshotT.NewSnapshot
  -> m (Either SnapshotT.SnapshotAlreadyExists SnapshotT.Snapshot)
create snapshotName snapshot = do
  path <- getSnapshotPath snapshotName
  HttpClient.authedJsonPutRequest path snapshot >>= \case
    Right r                         -> return $ Right r
    Left  e@(HttpT.ResponseError m) -> if HttpT.isAlreadyExistsError m
      then return $ Left SnapshotT.SnapshotAlreadyExists
      else throwM e
    Left e -> throwM e

createWithDefaults
  :: HttpT.PubSubHttpClientM m
  => SnapshotT.SnapshotName
  -> SubscriptionT.SubName
  -> m (Either SnapshotT.SnapshotAlreadyExists SnapshotT.Snapshot)
createWithDefaults snapshotName subName = do
  projectId <- HttpT.askProjectId
  let qualifiedSubName = SubscriptionT.qualifySubName projectId subName
      snapshot         = SnapshotT.minimalNewSnapshot qualifiedSubName
  create snapshotName snapshot

get
  :: HttpT.PubSubHttpClientM m => SnapshotT.SnapshotName -> m SnapshotT.Snapshot
get snapshotName = do
  path <- getSnapshotPath snapshotName
  HttpClient.authedJsonGetRequest path

delete :: HttpT.PubSubHttpClientM m => SnapshotT.SnapshotName -> m ()
delete snapshotName = do
  path <- getSnapshotPath snapshotName
  HttpClient.authedDeleteRequest path

patch
  :: HttpT.PubSubHttpClientM m
  => SnapshotT.SnapshotName
  -> SnapshotT.SnapshotPatch
  -> m SnapshotT.Snapshot
patch subName subPatch = do
  path <- getSnapshotPath subName
  HttpClient.authedJsonPatchRequest path subPatch >>= HttpT.getOrThrow

list
  :: HttpT.PubSubHttpClientM m
  => Maybe HttpT.PageQuery
  -> m SnapshotT.SnapshotListResponse
list pageQuery = do
  projectId <- Core.unwrapProjectId <$> HttpT.askProjectId
  let path = "/v1/projects/" <> Text.unpack projectId <> "/snapshots"
      pathQueryParams =
        HttpT.PathQueryParams path $ fmap HttpT.pageQueryParams pageQuery
  HttpClient.authedJsonGetRequest pathQueryParams
