module Test.Main where

import Prelude (class Show, Unit, bind, show, ($), (<$>), (*>), (<*>), (<>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE(), logShow)
import Control.Alt ((<|>))
import Data.List (List)
import Data.Map as M


import Routing (match)
import Routing.Match (Match, list)
import Routing.Match.Class (num, int, param, bool, lit, params)

data FooBar
  = Foo Number (M.Map String String)
  | Bar Boolean String
  | Baz (List Number)
  | Quux Int

instance showFooBar :: Show FooBar where
  show (Foo num q) = "(Foo " <> show num <> " " <> show q <> ")"
  show (Bar bool str) = "(Bar " <> show bool <> " " <> show str <> ")"
  show (Baz lst) = "(Baz " <> show lst <> ")"
  show (Quux i) = "(Quux " <> show i <> ")"

routing :: Match FooBar
routing =
  Foo <$> (lit "foo" *> num) <*> params
    <|> Bar <$> (lit "bar" *> bool) <*> (param "baz")
    <|> Quux <$> (lit "" *> lit "quux" *> int)
    <|> Baz <$> (list num)


main :: Eff (console :: CONSOLE) Unit
main = do
  logShow $ match routing "foo/12/?welp='hi'&b=false"
  logShow $ match routing "/quux/42"

  -- (minimal test for browser)

  -- matches routing $ \old new -> void do
  --   logShow old
  --   logShow new
