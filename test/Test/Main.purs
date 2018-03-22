module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.List as L
import Data.Map as M
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Routing (match)
import Routing.Match (Match, list, nonempty)
import Routing.Match.Class (bool, end, int, lit, num, str, param, params)
import Test.Assert (ASSERT, assertEqual)

data MyRoutes
  = Foo Number (M.Map String String)
  | Bar Boolean String
  | Baz (List Number)
  | Quux Int
  | Corge String
  | Corge' NonEmptyString
  | End Int

derive instance eqMyRoutes :: Eq MyRoutes
derive instance genericMyRoutes :: Generic MyRoutes _
instance showMyRoutes :: Show MyRoutes where show = genericShow

routing :: Match MyRoutes
routing = oneOf
  [ Foo <$> (lit "foo" *> num) <*> params
  , Bar <$> (lit "bar" *> bool) <*> (param "baz")
  , Baz <$> (lit "list" *> list num)
  , Quux <$> (lit "" *> lit "quux" *> int)
  , Corge <$> (lit "corge" *> str)
  , Corge' <$> (lit "corge'" *> nonempty)
  , End <$> (lit "" *> int <* end)
  ]

main :: Eff (assert :: ASSERT, console :: CONSOLE) Unit
main = do
  assertEqual
    { actual: match routing "foo/12/?welp='hi'&b=false"
    , expected: Right (Foo 12.0 (M.fromFoldable [Tuple "welp" "'hi'", Tuple "b" "false"]))
    }
  assertEqual
    { actual: match routing "foo/12?welp='hi'&b=false"
    , expected: Right (Foo 12.0 (M.fromFoldable [Tuple "welp" "'hi'", Tuple "b" "false"]))
    }
  assertEqual
    { actual: match routing "bar/true?baz=test"
    , expected: Right (Bar true "test")
    }
  assertEqual
    { actual: match routing "bar/false?baz=%D0%B2%D1%80%D0%B5%D0%BC%D0%B5%D0%BD%D0%BD%D1%8B%D0%B9%20%D1%84%D0%B0%D0%B9%D0%BB"
    , expected: Right (Bar false "временный файл")
    }
  assertEqual
    { actual: match routing "corge/test"
    , expected: Right (Corge "test")
    }
  assertEqual
    { actual: match routing "corge/%D0%B2%D1%80%D0%B5%D0%BC%D0%B5%D0%BD%D0%BD%D1%8B%D0%B9%20%D1%84%D0%B0%D0%B9%D0%BB"
    , expected: Right (Corge "временный файл")
    }
  assertEqual
    { actual: match routing "corge'/test"
    , expected: Right (Corge' (unsafePartial NES.unsafeFromString "test"))
    }
  assertEqual
    { actual: match routing "corge'/%D0%B2%D1%80%D0%B5%D0%BC%D0%B5%D0%BD%D0%BD%D1%8B%D0%B9%20%D1%84%D0%B0%D0%B9%D0%BB"
    , expected: Right (Corge' (unsafePartial NES.unsafeFromString "временный файл"))
    }
  assertEqual
    { actual: lmap (const unit) (match routing "corge'/")
    , expected: Left unit
    }
  assertEqual
    { actual: match routing "/quux/42"
    , expected: Right (Quux 42)
    }
  assertEqual
    { actual: match routing "list/123/"
    , expected: Right (Baz (L.fromFoldable [123.0]))
    }
  assertEqual
    { actual: match routing "list/123/456"
    , expected: Right (Baz (L.fromFoldable [123.0, 456.0]))
    }
  assertEqual
    { actual: match routing "list/"
    , expected: Right (Baz (L.fromFoldable []))
    }
  assertEqual
    { actual: match routing "list"
    , expected: Right (Baz (L.fromFoldable []))
    }
  assertEqual
    { actual: match routing "/1"
    , expected: Right (End 1)
    }
  assertEqual
    { actual: match routing "foo/0/?test=a/b/c"
    , expected: Right (Foo 0.0 (M.fromFoldable [Tuple "test" "a/b/c"]))
    }
