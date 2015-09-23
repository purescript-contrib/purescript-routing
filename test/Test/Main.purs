module Test.Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Alt
import Control.Apply
import Data.List


import Routing
import Routing.Match
import Routing.Match.Class

data FooBar = Foo Number | Bar Boolean String | Baz (List Number)

instance showFooBar :: Show FooBar where
  show (Foo num) = "(Foo " <> show num <> " )"
  show (Bar bool str) = "(Bar " <> show bool <> " " <> show str <> " )"
  show (Baz lst) = "(Baz " <> show lst <> " )"

routing :: Match FooBar
routing =
  Foo <$> (lit "foo" *> num)
  <|>
  Bar <$> (lit "bar" *> bool) <*> (param "baz")
  <|>
  Baz <$> (list num)


main = do
  print $ matchHash routing "foo/12"
  matches routing $ \old new -> void do
    print old
    print new
