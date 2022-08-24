module Pursley.Psb.IO
  ( openBinaryFile,
    catchDoesNotExist,
    saveBinaryFile,
  )
where

import Control.Exception (tryJust)
import Control.Monad (guard)
import qualified Data.ByteString as BS
import System.IO.Error (isDoesNotExistError)

openBinaryFile :: FilePath -> IO (Maybe BS.ByteString)
openBinaryFile = catchDoesNotExist . BS.readFile

saveBinaryFile :: FilePath -> BS.ByteString -> IO ()
saveBinaryFile = BS.writeFile

catchDoesNotExist :: IO a -> IO (Maybe a)
catchDoesNotExist inner = do
  r <- tryJust (guard . isDoesNotExistError) inner
  case r of
    Left () ->
      return Nothing
    Right x ->
      return (Just x)