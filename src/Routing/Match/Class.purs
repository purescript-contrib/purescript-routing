module Routing.Match.Class where

import Prelude (Unit)
import Control.Alternative (class Alternative)
import Data.Map as M

class (Alternative f) <= MatchClass f where
  -- | `lit x` will match exactly the path component `x`.
  -- | For example, `lit "x"` matches `/x`.
  lit :: String -> f Unit

  -- | `str` matches any path string component.
  -- | For example, `str` matches `/foo` as `"foo"`.
  str :: f String

  -- | `param p` matches a parameter assignment `q=v` within a query block.
  -- | For example, `param "q"` matches `/?q=a&r=b` as `"a"`.
  param :: String -> f String

  -- | `params` matches an entire query block. For exmaple, `params`
  -- | matches `/?q=a&r=b` as the map `{q : "a", r : "b"}`. Note that
  -- | `lit "foo" *> params` does *not* match `/foo`, since a query component
  -- | is *required*.
  params :: f (M.Map String String)

  -- | `num` matches any numerical path component.
  num :: f Number

  -- | `bool` matches any boolean path component.
  bool :: f Boolean

  fail :: forall a. String -> f a
