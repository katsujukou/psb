module Pursley.Psb.Options
  ( Options (..),
    mkOptions,
  )
where

import qualified Data.Text as T
import System.FilePath (takeExtension)

data Options = Options
  { optInput :: !FilePath,
    optFFIModule :: !(Maybe FilePath),
    optExterns :: ![FilePath],
    optOutputDir :: !FilePath,
    optSourcemaps :: !Bool,
    optPursleyCachedir :: !FilePath
  }
  deriving (Eq, Show)

mkOptions :: [FilePath] -> FilePath -> Bool -> FilePath -> Either T.Text Options
mkOptions inputs out sourcemaps cacheDir = do
  (input, ffi, externs) <- case inputs of
    [] -> do
      Left "You must give at least one input, which is path to purscript module to build."
    [h] -> pure (h, Nothing, [])
    (h : n : rest)
      | takeExtension n /= ".cbor" -> pure (h, Just n, rest)
      | otherwise -> pure (h, Nothing, n : rest)
  pure $ Options input ffi externs out sourcemaps cacheDir