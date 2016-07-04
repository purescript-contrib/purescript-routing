module Test.Main where

import Prelude (class Show, Unit, show, ($), (<$>), (*>), (<*>), (<>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE(), logShow)
import Control.Alt ((<|>))
import Data.List (List)
import Data.Map as M


import Routing (matchHash)
import Routing.Match (Match, list)
import Routing.Match.Class (num, param, bool, lit, params)

data FooBar = Foo Number (M.Map String String) | Bar Boolean String | Baz (List Number)

instance showFooBar :: Show FooBar where
  show (Foo num q) = "(Foo " <> show num <> " " <> show q <> ")"
  show (Bar bool str) = "(Bar " <> show bool <> " " <> show str <> ")"
  show (Baz lst) = "(Baz " <> show lst <> ")"

routing :: Match FooBar
routing =
  Foo <$> (lit "foo" *> num) <*> params
    <|> Bar <$> (lit "bar" *> bool) <*> (param "baz")
    <|> Baz <$> (list num)


main :: Eff (console :: CONSOLE) Unit
main = do
  logShow $ matchHash routing "foo/12/?welp='hi'&b=false"

  -- (minimal test for browser)

  -- matches routing $ \old new -> void do
  --   logShow old
  --   logShow new
