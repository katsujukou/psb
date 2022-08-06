module Psb where

import Control.Applicative (many)
import qualified Data.Text as T
import qualified Options.Applicative as Opts
import Pursley.Psb (runPsb)
import Pursley.Psb.Options (mkOptions)
import System.Exit (exitFailure)
import System.FilePath (takeExtension)
import qualified System.IO as IO
import Prelude

main :: IO ()
main = do
  (inputs, outdir, sourcemaps, cacheDir) <- Opts.execParser pinfo
  case mkOptions inputs outdir sourcemaps cacheDir of
    Left err -> do
      IO.hPutStrLn IO.stderr $ T.unpack err
      exitFailure
    Right opts -> do
      runPsb opts
  where
    pinfo :: Opts.ParserInfo ([FilePath], FilePath, Bool, FilePath)
    pinfo =
      Opts.info parser mempty

    parser :: Opts.Parser ([FilePath], FilePath, Bool, FilePath)
    parser =
      (,,,)
        <$> many (Opts.strArgument (Opts.metavar "INPUT"))
        <*> Opts.strOption (Opts.short 'o' <> Opts.metavar "OUTDIR")
        <*> Opts.switch (Opts.short 'M')
        <*> Opts.strOption (Opts.short 'c')