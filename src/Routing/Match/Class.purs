module Routing.Match.Class where

import Control.Alternative

class (Alternative f) <= MatchClass f where
  lit :: String -> f Unit
  str :: f String
  param :: String -> f String
  num :: f Number
  bool :: f Boolean
  fail :: forall a. String -> f a
