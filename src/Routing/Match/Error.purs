module Routing.Match.Error where

data MatchError
  -- expected other path part
  = UnexpectedPath String
    -- expected "true" or "false"
  | ExpectedBoolean
    -- expected numeric literal
  | ExpectedNumber
    -- expected string literal (found query probably or eol) 
  | ExpectedString
    -- expected query found path part or eol
  | ExpectedQuery
    -- expected path part found query or eol
  | ExpectedPathPart
    -- there is no such key in query
  | KeyNotFound String
    -- custom fail
  | Fail String 

showMatchError :: MatchError -> String
showMatchError err =
  case err of
    UnexpectedPath str -> "expected path part: " <> str
    KeyNotFound str -> "key: " <> str <> " has not found in query part"
    ExpectedQuery -> "expected query - found path"
    ExpectedNumber -> "expected number"
    ExpectedBoolean -> "expected boolean"
    ExpectedString -> "expected string var"
    ExpectedPathPart -> "expected path part, found query"
    Fail str -> "match error: " <> str
