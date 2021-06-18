{-# LANGUAGE ConstraintKinds #-}

module Cloud.PubSub.Http.Types
  ( TokenContainer(..)
  , ClientResources(..)
  , CloudTargetResources(..)
  , QueryParams(..)
  , HasPubSubHttpManager(..)
  , HasGoogleProjectId(..)
  , HasClientResources(..)
  , PathQueryParams(..)
  , PageToken
  , PageQuery(..)
  , PubSubHttpClientM
  , RequestError(..)
  , ErrorMessage(..)
  , ErrorRepsonse(..)
  , TargetResources(..)
  , pageQueryParams
  , isAlreadyExistsError
  , isInvalidArgumentError
  , simplePath
  ) where

import qualified Control.Monad.Logger          as ML
import qualified Cloud.PubSub.Auth.Types       as AuthT
import           Cloud.PubSub.Core.Types        ( ProjectId )
import           Control.Concurrent.MVar        ( MVar )
import           Control.Monad.Catch            ( Exception
                                                , MonadThrow
                                                )
import           Control.Monad.IO.Class         ( MonadIO )
import qualified Data.Aeson                    as Aeson
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as C8
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as TE
import           Data.Time                      ( NominalDiffTime )
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Conduit           ( Manager )

data TokenContainer = Available AuthT.CachedToken
                    | NotInitialized

data CloudTargetResources = CloudTargetResources
  { ctrServiceAccount  :: AuthT.ServiceAccount
  , ctrCachedTokenMVar :: MVar TokenContainer
  , ctrRenewThreshold  :: NominalDiffTime
  }

data TargetResources = Emulator
                     | Cloud CloudTargetResources

data ClientResources = ClientResources
  { crManager        :: Manager
  , crBaseUrl        :: String
  , crProjectId      :: ProjectId
  , crTargetResorces :: TargetResources
  }

newtype QueryParams = QueryParams {unwrapQueryParams :: [(ByteString, Maybe ByteString)]}
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

class HasClientResources m where
  askClientResources :: m ClientResources

class HasPubSubHttpManager m where
  askPubSubManger :: m Manager
  default askPubSubManger :: (HasClientResources m, Functor  m) => m Manager
  askPubSubManger = crManager <$> askClientResources
  askBaseUrl :: m String
  default askBaseUrl :: (HasClientResources m, Functor  m) => m String
  askBaseUrl = crBaseUrl <$> askClientResources

class HasGoogleProjectId m where
  askProjectId :: m ProjectId
  default askProjectId :: (HasClientResources m, Functor  m) => m ProjectId
  askProjectId = crProjectId <$> askClientResources

data PathQueryParams = PathQueryParams
  { path        :: String
  , queryParams :: Maybe QueryParams
  }
  deriving stock (Show, Eq)

simplePath :: String -> PathQueryParams
simplePath = flip PathQueryParams Nothing

type PubSubHttpClientM m
  = ( AuthT.GoogleApiAuth m
    , HasGoogleProjectId m
    , HasPubSubHttpManager m
    , ML.MonadLogger m
    , MonadIO m
    , MonadThrow m
    )

data ErrorMessage = ErrorMessage
  { code    :: Int
  , message :: Text
  , status  :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Aeson.FromJSON

isAlreadyExistsError :: ErrorMessage -> Bool
isAlreadyExistsError = (==) "ALREADY_EXISTS" . status

isInvalidArgumentError :: ErrorMessage -> Bool
isInvalidArgumentError = (==) "INVALID_ARGUMENT" . status

newtype ErrorRepsonse = ErrorRepsonse
  { error :: ErrorMessage
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Aeson.FromJSON

data RequestError = ResponseError ErrorMessage
                  | DecodeError Aeson.Value String
                    deriving stock (Show, Eq)

instance Exception RequestError

newtype PageToken = PageToken
  { unwrapPageToken :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

data PageQuery = PageQuery
  { pageSize  :: Int
  , pageToken :: Maybe PageToken
  }
  deriving stock (Show, Eq)

pageQueryParams :: PageQuery -> QueryParams
pageQueryParams pq = QueryParams
  [ ("pageSize" , Just $ C8.pack $ show $ pageSize pq)
  , ("pageToken", TE.encodeUtf8 . unwrapPageToken <$> pageToken pq)
  ]
