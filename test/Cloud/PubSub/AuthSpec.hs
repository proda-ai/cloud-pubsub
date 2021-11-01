module Cloud.PubSub.AuthSpec where

import qualified Cloud.PubSub.Auth             as Auth
import qualified Cloud.PubSub.Auth.Types       as AuthT
import qualified Network.HTTP.Client           as HttpClient
import qualified System.Environment            as SystemEnv
import qualified Network.HTTP.Client.TLS       as HttpClientTLS
import           Test.Hspec

tokenGetTest :: IO ()
tokenGetTest = do
  manager        <- HttpClient.newManager HttpClientTLS.tlsManagerSettings
  SystemEnv.lookupEnv "GOOGLE_APPLICATION_CREDENTIALS" >>= \case
    Nothing -> pendingWith "skipping as auth not supported in emulator"
    Just serviceAccountPath -> do
      serviceAccount <- Auth.readServiceAccountFile serviceAccountPath
      tokenResponse  <- Auth.fetchToken manager serviceAccount scope
      AuthT.tokenType tokenResponse `shouldBe` "Bearer"
      AuthT.expiresIn tokenResponse `shouldSatisfy` (\t -> 3595 < t && t < 3600)
      where scope = "https://www.googleapis.com/auth/pubsub"

spec :: Spec
spec = describe "Google API Auth" $ do
  it "can fetch a token" tokenGetTest
