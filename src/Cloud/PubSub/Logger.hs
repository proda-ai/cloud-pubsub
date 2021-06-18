module Cloud.PubSub.Logger where

import           Control.Monad.Logger           ( MonadLogger )
import qualified Control.Monad.Logger          as ML
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Lazy          as LBS
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as TE

type LoggerFn = ML.Loc -> ML.LogSource -> ML.LogLevel -> ML.LogStr -> IO ()

data LogLevel = Debug | Info | Warn | Error deriving (Show, Eq)

logWithContext
  :: MonadLogger m => LogLevel -> Maybe Aeson.Value -> Text -> m ()
logWithContext level ctx msg = case level of
  Debug -> ML.logDebugN contents
  Info  -> ML.logDebugN contents
  Warn  -> ML.logDebugN contents
  Error -> ML.logDebugN contents
 where
  contents =
    " " <> msg <> " " <> TE.decodeUtf8 (LBS.toStrict $ Aeson.encode ctx)
