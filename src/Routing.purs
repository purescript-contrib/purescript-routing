module Routing
  ( match
  , matchWith
  ) where

import Prelude

import Data.Either (Either)
import Global (decodeURIComponent)
import Routing.Match (Match, runMatch)
import Routing.Parser (parse)

-- | Runs a `Match` parser.
match :: forall a. Match a -> String -> Either String a
match = matchWith decodeURIComponent

-- | Runs a `Match` parser given a custom String decoder.
matchWith :: forall a. (String -> String) -> Match a -> String -> Either String a
matchWith decoder matcher = runMatch matcher <<< parse decoder
