module Test.Main where

import Prelude (class Show, Unit, discard, show, ($), (<$>), (*>), (<*), (<*>), (<>), append)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Alt ((<|>))
import Data.List (List)
import Data.Map as M


import Routing (match)
import Routing.Match (Match, list)
import Routing.Match.Class (bool, end, int, lit, num, param, params)

data FooBar
  = Foo Number (M.Map String String)
  | Bar Boolean String
  | Baz (List Number)
  | Quux Int
  | End Int

instance showFooBar :: Show FooBar where
  show (Foo num q) = "(Foo " <> show num <> " " <> show q <> ")"
  show (Bar bool str) = "(Bar " <> show bool <> " " <> show str <> ")"
  show (Baz lst) = "(Baz " <> show lst <> ")"
  show (Quux i) = "(Quux " <> show i <> ")"
  show (End i) = "(End " <> show i <> ")"

routing :: Match FooBar
routing =
  Foo <$> (lit "foo" *> num) <*> params
    <|> Bar <$> (lit "bar" *> bool) <*> (param "baz")
    <|> Quux <$> (lit "" *> lit "quux" *> int)
    -- Order matters here.  `list` is greedy, and `end` wont match after it
    <|> End <$> (lit "" *> int <* end)
    <|> Baz <$> (list num)


main :: Eff (console :: CONSOLE) Unit
main = do
  print "Foo: " $ match routing "foo/12/?welp='hi'&b=false" -- foo
  print "Foo: " $ match routing "foo/12?welp='hi'&b=false" -- foo
  print "Quux: " $ match routing "/quux/42" -- quux
  print "Baz: " $ match routing "/123/" -- baz
  print "End: " $ match routing "/1" -- end

  where print s e = log $ append s $ show e

  -- (minimal test for browser)

  -- matches routing $ \old new -> void do
  --   logShow old
  --   logShow new
