module Routing.Match.Class where

import Control.MonadPlus

class (MonadPlus f) <= MatchClass f where
  lit :: String -> f Unit
  var :: f String
  param :: String -> f String
  fail :: forall a. String -> f a
