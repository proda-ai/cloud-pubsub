module Cloud.PubSub.HttpClient
  ( authedDeleteRequest
  , authedJsonBodyRequest
  , authedJsonGetRequest
  , authedJsonPatchRequest
  , authedJsonPostRequest
  , authedJsonPutRequest
  , authedNoBodyPostRequest
  , authedNoContentPostRequest
  ) where

import qualified Cloud.PubSub.Auth.Types       as AuthT
import qualified Cloud.PubSub.Http.Retry       as Retry
import qualified Cloud.PubSub.Http.Types       as HttpT
import           Control.Monad.Catch            ( MonadThrow
                                                , throwM
                                                )
import qualified Data.Aeson                    as Aeson
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as C8
import           Data.Functor                   ( (<&>)
                                                , void
                                                )
import qualified Data.Text.Encoding            as TE
import           Network.HTTP.Client.Conduit    ( RequestBody(RequestBodyLBS) )
import qualified Network.HTTP.Conduit          as Http
import qualified Network.HTTP.Simple           as Http
import qualified Network.HTTP.Types.Status     as Status

addAuthHeader :: AuthT.AccessToken -> Http.Request -> Http.Request
addAuthHeader token request =
  let headerValue = TE.encodeUtf8 $ "Bearer " <> AuthT.unwrapAccessToken token
  in  Http.addRequestHeader "authorization" headerValue request

authRequest :: HttpT.PubSubHttpClientM m => Http.Request -> m Http.Request
authRequest request = AuthT.getToken <&> \case
  Just token -> addAuthHeader token request
  Nothing    -> request

authJsonRequest
  :: (HttpT.PubSubHttpClientM m, Aeson.FromJSON r)
  => Http.Request
  -> m (Http.Response r)
authJsonRequest request = authRequest request >>= Http.httpJSON

mkRequest
  :: (MonadThrow m, HttpT.HasPubSubHttpManager m)
  => ByteString
  -> HttpT.PathQueryParams
  -> m Http.Request
mkRequest method (HttpT.PathQueryParams path mayQueryParams) = do
  baseUrl <- HttpT.askBaseUrl
  parsed  <- Http.parseRequest $ baseUrl <> path
  manager <- HttpT.askPubSubManger
  let baseReq =
        Http.setRequestMethod method $ Http.setRequestManager manager parsed
  return $ maybe baseReq
                 ((`Http.setQueryString` baseReq) . HttpT.unwrapQueryParams)
                 mayQueryParams

authedJsonGetRequest
  :: (HttpT.PubSubHttpClientM m, Aeson.FromJSON r)
  => HttpT.PathQueryParams
  -> m r
authedJsonGetRequest pathQueryParams = do
  request <- mkRequest "GET" pathQueryParams
  (Retry.httpClientRetry name $ runRequest request) >>= either throwM return
 where
  name       = "GET: " <> HttpT.path pathQueryParams
  runRequest = fmap decodeResponse . authJsonRequest

decodeResponse
  :: Aeson.FromJSON a
  => Http.Response Aeson.Value
  -> Either HttpT.RequestError a
decodeResponse response = if Status.statusIsSuccessful status
  then case Aeson.fromJSON body of
    Aeson.Success v -> Right v
    Aeson.Error   e -> Left $ HttpT.DecodeError body e
  else case Aeson.fromJSON body of
    Aeson.Success (HttpT.ErrorRepsonse v) ->
      Left $ HttpT.ErrorResponseError status v
    Aeson.Error e -> Left $ HttpT.OtherError status e
 where
  body   = Http.getResponseBody response
  status = Http.getResponseStatus response

authedJsonBodyRequest
  :: (HttpT.PubSubHttpClientM m, Aeson.ToJSON b, Aeson.FromJSON r)
  => ByteString
  -> HttpT.PathQueryParams
  -> b
  -> m (Either HttpT.RequestError r)
authedJsonBodyRequest method pathQueryParams bodyValue = do
  request <- mkRequest method pathQueryParams
  let body = RequestBodyLBS $ Aeson.encode bodyValue
      name = C8.unpack method <> ": " <> HttpT.path pathQueryParams
  Retry.httpClientRetry name $ do
    response <-
      authJsonRequest
      $ Http.setRequestBody body
      $ Http.setRequestResponseTimeout timeout request
    return $ decodeResponse response
 where
  minute  = 60 * 1000 * 1000
  timeout = Http.responseTimeoutMicro minute

authedJsonPutRequest
  :: (HttpT.PubSubHttpClientM m, Aeson.ToJSON b, Aeson.FromJSON r)
  => HttpT.PathQueryParams
  -> b
  -> m (Either HttpT.RequestError r)
authedJsonPutRequest = authedJsonBodyRequest "PUT"

authedJsonPatchRequest
  :: (HttpT.PubSubHttpClientM m, Aeson.ToJSON b, Aeson.FromJSON r)
  => HttpT.PathQueryParams
  -> b
  -> m (Either HttpT.RequestError r)
authedJsonPatchRequest = authedJsonBodyRequest "PATCH"

authedJsonPostRequest
  :: (HttpT.PubSubHttpClientM m, Aeson.ToJSON b, Aeson.FromJSON r)
  => HttpT.PathQueryParams
  -> b
  -> m (Either HttpT.RequestError r)
authedJsonPostRequest = authedJsonBodyRequest "POST"

authedNoBodyPostRequest
  :: (HttpT.PubSubHttpClientM m) => HttpT.PathQueryParams -> m ()
authedNoBodyPostRequest pathQueryParams = do
  request       <- mkRequest "POST" pathQueryParams
  authedRequest <- authRequest request
  void $ Http.httpNoBody authedRequest

authedNoContentPostRequest
  :: (HttpT.PubSubHttpClientM m, Aeson.ToJSON b)
  => HttpT.PathQueryParams
  -> b
  -> m ()
authedNoContentPostRequest pathQueryParams bodyValue = do
  parsedRequest <- mkRequest "POST" pathQueryParams
  let body    = RequestBodyLBS $ Aeson.encode bodyValue
      request = Http.setRequestBody body parsedRequest
  authedRequest <- authRequest request
  void $ Http.httpNoBody authedRequest

authedDeleteRequest
  :: HttpT.PubSubHttpClientM m => HttpT.PathQueryParams -> m ()
authedDeleteRequest pathQueryParams = do
  request       <- mkRequest "DELETE" pathQueryParams
  authedRequest <- authRequest request
  void $ Http.httpNoBody authedRequest
