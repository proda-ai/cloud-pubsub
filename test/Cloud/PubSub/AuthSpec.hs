module Cloud.PubSub.AuthSpec where

import qualified Cloud.PubSub.Auth             as Auth
import qualified Cloud.PubSub.Auth.Types       as AuthT
import qualified Data.Aeson                   as Aeson
import qualified Data.Text                     as Text
import qualified Network.HTTP.Client           as HttpClient
import qualified System.Environment            as SystemEnv
import qualified Network.HTTP.Client.TLS       as HttpClientTLS
import           Test.Hspec

tokenGetTest :: IO ()
tokenGetTest = do
  manager        <- HttpClient.newManager HttpClientTLS.tlsManagerSettings
  SystemEnv.lookupEnv "PUBSUB_EMULATOR_HOST" >>= \case
    Just _ -> pendingWith "skipping as auth not supported in emulator"
    Nothing -> do
      SystemEnv.lookupEnv "GOOGLE_APPLICATION_CREDENTIALS" >>= \case
        Just credPath -> do
          -- Try ADC first
          adcResult <- Aeson.eitherDecodeFileStrict credPath :: IO (Either String (Auth.ADCCredentials))
          case adcResult of
            Right adcCreds | Auth.adcType adcCreds == Text.pack "authorized_user" -> do
              adc <- Auth.readApplicationDefaultCredentialsFile credPath
              tokenResponse <- Auth.fetchApplicationDefaultCredentialsToken manager adc scope
              AuthT.tokenType tokenResponse `shouldBe` "Bearer"
              AuthT.expiresIn tokenResponse `shouldSatisfy` (\t -> 3595 < t && t < 3600)
            _ -> do
              -- Try as ServiceAccount
              serviceAccount <- Auth.readServiceAccountFile credPath
              tokenResponse  <- Auth.fetchToken manager serviceAccount scope
              AuthT.tokenType tokenResponse `shouldBe` "Bearer"
              AuthT.expiresIn tokenResponse `shouldSatisfy` (\t -> 3595 < t && t < 3600)
        Nothing -> do
          -- Try metadata server
          tokenResponse <- Auth.fetchMetadataToken manager scope
          AuthT.tokenType tokenResponse `shouldBe` "Bearer"
          AuthT.expiresIn tokenResponse `shouldSatisfy` (\t -> 3595 < t && t < 3600)
      where scope = "https://www.googleapis.com/auth/pubsub"

spec :: Spec
spec = describe "Google API Auth" $ do
  it "can fetch a token" tokenGetTest
