module Cloud.PubSub.Auth
  ( fetchToken
  , readServiceAccountFile
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
import qualified Data.Text                     as Text
import qualified Data.Time                     as Time
import qualified Network.HTTP.Client.Conduit   as HttpClientC
import qualified Network.HTTP.Simple           as HTTP

readServiceAccountFile :: MonadIO m => FilePath -> m AuthT.ServiceAccount
readServiceAccountFile fp =
  liftIO $ Aeson.eitherDecodeFileStrict fp >>= either fail return

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
