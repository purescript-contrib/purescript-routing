module Main where

import Control.Monad.Eff
import Debug.Trace 
import Control.Alt
import Control.Apply
import Debug.Foreign


import Routing
import Routing.Match
import Routing.Match.Class
import Routing.Match.Combinators

data FooBar = Foo Number | Bar Boolean String

routing :: Match FooBar
routing =
  Foo <$> (lit "foo" *> (var >>= num))
  <|>
  Bar <$> (lit "bar" *> (var >>= bool)) <*> (param "baz")

main = do
  fprint $ matchHash routing "foo/asdf"
  matches routing $ \old new -> void $ do
    fprint old
    fprint new

