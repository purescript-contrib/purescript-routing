module Routing.Match.Error where

import Prelude ((<>))

data MatchError
  = UnexpectedPath String
  | ExpectedBoolean
  | ExpectedEnd
  | ExpectedNumber
  | ExpectedInt
  | ExpectedString
  | ExpectedQuery
  | ExpectedPathPart
  | KeyNotFound String
  | Fail String

showMatchError :: MatchError -> String
showMatchError err =
  case err of
    UnexpectedPath str -> "expected path part: " <> str
    KeyNotFound str -> "key: " <> str <> " has not found in query part"
    ExpectedQuery -> "expected query - found path"
    ExpectedNumber -> "expected number"
    ExpectedInt -> "expected int"
    ExpectedBoolean -> "expected boolean"
    ExpectedEnd -> "expected end"
    ExpectedString -> "expected string var"
    ExpectedPathPart -> "expected path part, found query"
    Fail str -> "match error: " <> str
