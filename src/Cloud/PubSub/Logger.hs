module Cloud.PubSub.Logger where

import           Control.Monad                  ( when )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Char8         as C8
import qualified Data.ByteString.Lazy          as LBS

data Logger = Logger
  { logLevel   :: Maybe LogLevel
  , logMessage :: String -> IO ()
  }

data LogLevel = Debug | Info | Warn | Error deriving (Show, Eq, Ord )

class HasLogger m where
  askLogger  :: m Logger

logStructuredStr :: LogLevel -> Aeson.Value -> String -> String
logStructuredStr level ctx msg =
  show level <> " " <> msg <> " " <> C8.unpack (LBS.toStrict $ Aeson.encode ctx)

verboseLog
  :: MonadIO m => Logger -> LogLevel -> Maybe Aeson.Value -> String -> m ()
verboseLog logger level maybeCtx msg = case logLevel logger of
  Nothing          -> return ()
  Just loggerLevel -> when (level >= loggerLevel) $ do
    liftIO $ logMessage logger $ do
      case maybeCtx of
        Just ctx -> logStructuredStr level ctx msg
        Nothing  -> msg

log
  :: (MonadIO m, HasLogger m) => LogLevel -> Maybe Aeson.Value -> String -> m ()
log level maybeCtx msg = do
  logger <- askLogger
  verboseLog logger level maybeCtx msg
