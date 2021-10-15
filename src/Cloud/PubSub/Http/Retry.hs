{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}

module Cloud.PubSub.Http.Retry
  ( httpClientRetry
  ) where

import qualified Cloud.PubSub.Http.Types       as HttpT
import           Control.Monad                  ( when )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Logger           ( MonadLogger
                                                , logError
                                                )
import qualified Control.Retry                 as Retry
import qualified Data.Text                     as Text
import qualified Network.HTTP.Types.Status     as Status

retryPolicy :: MonadIO m => Retry.RetryPolicyM m
retryPolicy = Retry.limitRetriesByCumulativeDelay totalDelayMicros
  $ Retry.fullJitterBackoff baseDelayMicros
 where
  baseDelayMicros  = 50_000
  totalDelayMicros = 5_000_000

retryDecider :: HttpT.RequestError -> Bool
retryDecider = \case
  HttpT.ResponseError status _ -> status `elem` retryStatuses
  HttpT.DecodeError status _ _ -> status `elem` retryStatuses
 where
  -- Based on what is described to be safe to retry
  -- https://cloud.google.com/pubsub/docs/reference/error-codes
  retryStatuses =
    [ Status.badGateway502
    , Status.internalServerError500
    , Status.tooManyRequests429
    , Status.serviceUnavailable503
    ]

retryCheck
  :: Monad m => Retry.RetryStatus -> Either HttpT.RequestError a -> m Bool
retryCheck _ = \case
  Right _   -> pure False
  Left  err -> pure $ retryDecider err

httpClientRetry
  :: forall m a
   . (MonadIO m, MonadLogger m)
  => String
  -> m (Either HttpT.RequestError a)
  -> m (Either HttpT.RequestError a)
httpClientRetry name action = Retry.retrying retryPolicy retryCheck mkAttempt
 where
  logAttempt :: Int -> m ()
  logAttempt n =
    $logError $ Text.pack $ mconcat ["retrying request ", name, " (", show n, ")"]

  mkAttempt :: Retry.RetryStatus -> m (Either HttpT.RequestError a)
  mkAttempt rs = do
    when (n > 0) (logAttempt n)
    action
    where n = Retry.rsIterNumber rs
