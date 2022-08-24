module Pursley.Psb.Depfile
  ( openDepfile
  , depfilename
  , DepFile (..)
  ) where

import Codec.Serialise (Serialise)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import qualified Language.PureScript as PS
import Pursley.Psb.Error (PsbError (InvalidDepfile))
import System.IO.Error (tryIOError)

data DepFile = DepFile
  { dpfCompilerVersion :: !T.Text,
    dpfEntries :: ![(FilePath, UTCTime)]
  }
  deriving (Eq, Show, Generic)

instance Serialise DepFile

openDepfile :: (MonadIO m, MonadError PsbError m) => FilePath -> m (Maybe DepFile)
openDepfile fp = do
  res <- liftIO (tryIOError $ PS.readCborFileIO fp)
  either (throwError . InvalidDepfile fp) pure res

depfilename :: FilePath
depfilename = "depfile"