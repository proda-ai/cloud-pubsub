module Cloud.PubSub.Logger
  ( LoggerFn
  , logWithContext
  ) where

import           Control.Monad.Logger           ( MonadLogger )
import qualified Control.Monad.Logger          as ML
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Lazy          as LBS
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as TE

type LoggerFn = ML.Loc -> ML.LogSource -> ML.LogLevel -> ML.LogStr -> IO ()

logWithContext
  :: MonadLogger m => ML.LogLevel -> Maybe Aeson.Value -> Text -> m ()
logWithContext level ctx msg = ML.logOtherN level contents
 where
  contents =
    " " <> msg <> " " <> TE.decodeUtf8 (LBS.toStrict $ Aeson.encode ctx)
