module Pursley.Psb.Error
  ( PsbError (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Language.PureScript as P
import qualified Language.PureScript.CST as P
import Prelude

data PsbError
  = PursError P.MultipleErrors
  | PursParseErrors (NonEmpty P.ParserError)
  | InvalidExternsFile FilePath
  | InternalBuildError T.Text
  deriving (Show, Generic)