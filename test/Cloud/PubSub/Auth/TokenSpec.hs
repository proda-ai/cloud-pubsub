module Cloud.PubSub.Auth.TokenSpec where

import qualified Cloud.PubSub.Auth.Types       as AuthT
import           Cloud.PubSub.TestHelpers       ( mkTestPubSubEnv
                                                , mkTestPubSubEnvWithRenewThreshold
                                                , runTestIfNotEmulator
                                                )
import qualified Control.Concurrent            as Concurrent
import           Control.Monad.IO.Class         ( liftIO )
import           Test.Hspec

getTokenOrFail :: (AuthT.GoogleApiAuth m, MonadFail m) => m AuthT.AccessToken
getTokenOrFail = AuthT.getToken >>= \case
  Just t  -> return t
  Nothing -> fail "expected cloud test to obtain token"

cachedTokenTest :: IO ()
cachedTokenTest = do
  env <- mkTestPubSubEnv
  flip runTestIfNotEmulator env $ do
    token1 <- getTokenOrFail
    token2 <- getTokenOrFail
    liftIO $ token1 `shouldBe` token2

newTokenAfterExpiryTest :: IO ()
newTokenAfterExpiryTest = do
  env <- mkTestPubSubEnvWithRenewThreshold (3600 - 3)
  flip runTestIfNotEmulator env $ do
    token1 <- getTokenOrFail
    liftIO $ Concurrent.threadDelay $ secToMicro 5
    token2 <- getTokenOrFail
    liftIO $ token1 `shouldNotBe` token2
  where secToMicro n = n * 1000 * 1000

spec :: Spec
spec = do
  describe "Token" $ do
    it "can get a cached token"                          cachedTokenTest
    it "can get a new a token after the old one expires" newTokenAfterExpiryTest
