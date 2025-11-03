module Cloud.PubSub.Auth
  ( fetchToken
  , readServiceAccountFile
  , fetchMetadataToken
  , readApplicationDefaultCredentialsFile
  , fetchApplicationDefaultCredentialsToken
  , ADCCredentials(..)
  ) where

import qualified Cloud.PubSub.Auth.Types       as AuthT
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Crypto.Hash.Algorithms         ( SHA256(..) )
import           Crypto.PubKey.RSA              ( PrivateKey )
import qualified Crypto.PubKey.RSA.PKCS15      as RSA
import qualified Data.Aeson                    as Aeson
import           Data.Aeson                     ( (.=) )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Base64.URL    as Base64
import qualified Data.ByteString.Lazy          as LBS
import           Data.Functor                   ( (<&>) )
import           GHC.Generics                   ( Generic )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as TE
import qualified Data.Time                     as Time
import qualified Network.HTTP.Client.Conduit   as HttpClientC
import qualified Network.HTTP.Simple           as HTTP

readServiceAccountFile :: MonadIO m => FilePath -> m AuthT.ServiceAccount
readServiceAccountFile fp =
  liftIO $ Aeson.eitherDecodeFileStrict fp >>= either fail return

data ADCCredentials = ADCCredentials
  { adcType         :: Text.Text
  , adcClientId     :: Text.Text
  , adcClientSecret :: Text.Text
  , adcRefreshToken :: Text.Text
  }
  deriving stock (Generic, Show)

instance Aeson.FromJSON ADCCredentials where
  parseJSON = Aeson.genericParseJSON $ Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 3
    }

readApplicationDefaultCredentialsFile :: MonadIO m => FilePath -> m AuthT.ApplicationDefaultCredentials
readApplicationDefaultCredentialsFile fp = liftIO $ do
  result <- Aeson.eitherDecodeFileStrict fp :: IO (Either String ADCCredentials)
  case result of
    Right adc | adcType adc == Text.pack "authorized_user" ->
      return $ AuthT.ApplicationDefaultCredentials
        { AuthT.adcClientId     = adcClientId adc
        , AuthT.adcClientSecret = adcClientSecret adc
        , AuthT.adcRefreshToken = adcRefreshToken adc
        }
    Right adc ->
      fail $ "Unsupported ADC credentials type: " <> Text.unpack (adcType adc) <>
             ". Only 'authorized_user' type is supported. For WIF/impersonated service accounts," <>
             " please use environment variables or a supported credential format."
    Left err -> do
      -- Try to read as raw JSON to provide better error message
      rawResult <- Aeson.eitherDecodeFileStrict fp :: IO (Either String Aeson.Value)
      case rawResult of
        Right (Aeson.Object obj) ->
          case Aeson.lookup "type" obj of
            Just (Aeson.String credType) ->
              fail $ "Unsupported credential type: " <> Text.unpack credType <>
                     ". Expected 'authorized_user' ADC format with client_id, client_secret, and refresh_token fields."
            _ ->
              fail $ "Could not parse credentials file. Expected ADC format with 'authorized_user' type. " <>
                     "Parse error: " <> err
        _ ->
          fail $ "Could not parse credentials file as JSON. Parse error: " <> err

createAssertionTokenBody
  :: AuthT.PrivateKeyId -> AuthT.TokenClaims -> ByteString
createAssertionTokenBody (AuthT.PrivateKeyId keyId) tokenClaims =
  let header = Aeson.object
        [ "alg" .= Aeson.String "RS256"
        , "kid" .= Aeson.String keyId
        , "typ" .= Aeson.String "JWT"
        ]
  in  encode header <> "." <> encode (Aeson.toJSON tokenClaims)
  where encode = Base64.encodeUnpadded . LBS.toStrict . Aeson.encode

-- The format of a JWT is decribed here
-- https://jwt.io/introduction
createAssertionToken
  :: PrivateKey -> AuthT.PrivateKeyId -> AuthT.TokenClaims -> IO ByteString
createAssertionToken key keyId claims = do
  signature <- RSA.signSafer (Just SHA256) key tokenBody >>= \case
    Left  e -> fail $ "could not sign token " <> show e
    Right s -> return s
  return $ tokenBody <> "." <> Base64.encodeUnpadded signature
  where tokenBody = createAssertionTokenBody keyId claims

fetchToken
  :: MonadIO m
  => HttpClientC.Manager
  -> AuthT.ServiceAccount
  -> AuthT.Scope
  -> m AuthT.AccessTokenResponse
fetchToken manager serviceAccount scope = liftIO $ do
  -- implements the flow described here,
  -- https://developers.google.com/identity/protocols/oauth2/service-account
  now <- Time.getCurrentTime
  let oneHour = 3600
      email   = AuthT.saClientEmail serviceAccount
      claims  = AuthT.TokenClaims
        { issuedAt = AuthT.UnixEpochSeconds now
        , expiryAt = AuthT.UnixEpochSeconds $ Time.addUTCTime oneHour now
        , audience = Text.pack googleTokenUrl
        , issuer   = email
        , subject  = email
        , scope    = scope
        }
      keyId = AuthT.saPrivateKeyId serviceAccount
      key   = AuthT.unwrapX509PrivateKey $ AuthT.saPrivateKey serviceAccount
  token   <- createAssertionToken key keyId claims
  request <-
    HTTP.parseRequest ("POST " <> googleTokenUrl)
      <&> ( HTTP.setRequestBodyURLEncoded (formData token)
          . HTTP.setRequestManager manager
          )
  response <- HTTP.httpJSON request
  return $ HTTP.getResponseBody response
 where
  googleTokenUrl :: String
  googleTokenUrl = "https://www.googleapis.com/oauth2/v4/token"

  formData :: ByteString -> [(ByteString, ByteString)]
  formData token =
    [ ("grant_type", "urn:ietf:params:oauth:grant-type:jwt-bearer")
    , ("assertion" , token)
    ]

fetchMetadataToken
  :: MonadIO m
  => HttpClientC.Manager
  -> AuthT.Scope
  -> m AuthT.AccessTokenResponse
fetchMetadataToken manager scope = liftIO $ do
  -- Implements the metadata server flow:
  -- https://cloud.google.com/docs/authentication/rest#metadata-server
  -- Pass requested OAuth scope via query string. The client will URL-encode values.
  let metadataTokenUrl =
        "http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token"
      query = [("scopes", Just (TE.encodeUtf8 (AuthT.unwrapScope scope)))]
  request <-
    HTTP.parseRequest ("GET " <> metadataTokenUrl)
      <&> ( HTTP.setRequestQueryString query
          . HTTP.addRequestHeader "Metadata-Flavor" "Google"
          . HTTP.setRequestManager manager
          )
  response <- HTTP.httpJSON request
  return $ HTTP.getResponseBody response

-- | Fetches a GCP access token using Application Default Credentials (from gcloud auth application-default login)
-- Implements the OAuth2 refresh token flow described here:
-- https://developers.google.com/identity/protocols/oauth2/web-server#offline
fetchApplicationDefaultCredentialsToken
  :: MonadIO m
  => HttpClientC.Manager
  -> AuthT.ApplicationDefaultCredentials
  -> AuthT.Scope
  -> m AuthT.AccessTokenResponse
fetchApplicationDefaultCredentialsToken manager adc scope = liftIO $ do
  let tokenUrl = "https://oauth2.googleapis.com/token"
      formData =
        [ ("client_id"    , TE.encodeUtf8 $ AuthT.adcClientId adc)
        , ("client_secret", TE.encodeUtf8 $ AuthT.adcClientSecret adc)
        , ("refresh_token" , TE.encodeUtf8 $ AuthT.adcRefreshToken adc)
        , ("grant_type"   , "refresh_token")
        , ("scope"        , TE.encodeUtf8 $ AuthT.unwrapScope scope)
        ]
  request <-
    HTTP.parseRequest ("POST " <> tokenUrl)
      <&> ( HTTP.setRequestBodyURLEncoded formData
          . HTTP.setRequestManager manager
          )
  response <- HTTP.httpJSON request
  return $ HTTP.getResponseBody response
