module Routing.Match.Combinators where

import Routing.Match.Class
import Global (readFloat, isNaN)

num :: forall f. (MatchClass f) => String -> f Number
num input =
  let res = readFloat input in
  case isNaN res of
    true -> fail "not a number" 
    false -> return res

bool :: forall f. (MatchClass f) => String -> f Boolean
bool input =
  case input of
    "true" -> pure true
    "false" -> pure false
    _ -> fail "not a boolean"
