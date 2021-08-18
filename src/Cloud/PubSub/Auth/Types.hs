module Cloud.PubSub.Auth.Types
  ( AccessToken(..)
  , AccessTokenResponse(..)
  , CachedToken(..)
  , GoogleApiAuth(..)
  , PrivateKeyId(..)
  , Scope(..)
  , ServiceAccount(..)
  , TokenClaims(..)
  , UnixEpochSeconds(..)
  , X509PrivateKey(..)
  ) where

import           Crypto.PubKey.RSA.Types        ( PrivateKey )
import qualified Data.Aeson                    as Aeson
import           Data.Aeson                     ( (.=) )
import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as TE
import           Data.Time                      ( NominalDiffTime
                                                , UTCTime
                                                )
import qualified Data.Time.Clock.POSIX         as POSIX
import qualified Data.X509                     as X509
import qualified Data.X509.Memory              as X509
import           GHC.Generics                   ( Generic )

newtype PrivateKeyId = PrivateKeyId {unwrapPrivateKeyId :: Text}
  deriving stock (Show)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

newtype Scope = Scope {unwrapScope :: Text}
  deriving stock (Show)
  deriving newtype (Aeson.ToJSON, IsString, Aeson.FromJSON)

newtype X509PrivateKey = X509PrivateKey { unwrapX509PrivateKey :: PrivateKey }

instance Aeson.FromJSON X509PrivateKey where
  parseJSON = Aeson.withText "X509PrivateKey" $ \text -> do
    case X509.readKeyFileFromMemory $ TE.encodeUtf8 text of
      [X509.PrivKeyRSA k] -> return $ X509PrivateKey k
      _                   -> fail "expected a single RSA private key"

data ServiceAccount = ServiceAccount
  { saType                    :: Text
  -- Given that service accounts can be used acrooss projects 
  -- "project_id" is ignored
  , saPrivateKeyId            :: PrivateKeyId
  , saPrivateKey              :: X509PrivateKey
  , saClientEmail             :: Text
  , saClientId                :: Text
  , saAuthUri                 :: Text
  , saTokenUri                :: Text
  , saAuthProviderX509CertUrl :: Text
  , saClientX509CertUrl       :: Text
  }
  deriving stock Generic

instance Aeson.FromJSON ServiceAccount where
  parseJSON = Aeson.genericParseJSON $ Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
    }

newtype AccessToken = AccessToken
  { unwrapAccessToken :: Text
  }
  deriving stock (Show, Eq)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

data AccessTokenResponse = AccessTokenResponse
  { accessToken :: AccessToken
  , tokenType   :: Text
  , expiresIn   :: NominalDiffTime
  }
  deriving stock Generic

instance Aeson.FromJSON AccessTokenResponse where
  parseJSON = Aeson.genericParseJSON
    $ Aeson.defaultOptions { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' }

data CachedToken = CachedToken
  { accessToken :: AccessToken
  , expiresAt   :: UTCTime
  }

newtype UnixEpochSeconds = UnixEpochSeconds
  { unwrapUnixEpochSeconds :: UTCTime }

unixSeconds :: Fractional a => UTCTime -> a
unixSeconds = fromRational . toRational . POSIX.utcTimeToPOSIXSeconds

instance Aeson.ToJSON UnixEpochSeconds where
  toJSON = Aeson.Number . unixSeconds . unwrapUnixEpochSeconds

data TokenClaims = TokenClaims
  { issuer   :: Text
  , subject  :: Text
  , scope    :: Scope
  , audience :: Text
  , issuedAt :: UnixEpochSeconds
  , expiryAt :: UnixEpochSeconds
  }

instance Aeson.ToJSON TokenClaims where
  toJSON tc = Aeson.object
    [ "iss" .= issuer tc
    , "sub" .= subject tc
    , "scope" .= scope tc
    , "aud" .= audience tc
    , "iat" .= issuedAt tc
    , "exp" .= expiryAt tc
    ]

class Monad m => GoogleApiAuth m where
  getToken :: m (Maybe AccessToken)
