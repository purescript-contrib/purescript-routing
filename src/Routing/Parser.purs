module Routing.Parser (
  parse
  ) where

import Prelude ((>>>), map, ($), (<<<), (==), bind, (<*>), (<$>), (<=))
import Control.MonadPlus (guard)
import Data.Maybe (Maybe(), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.List (toList, List())
import Data.Traversable (traverse)
import Data.Map as M
import Data.String as S
import Data.Array as A

import Routing.Types (Route, RoutePart(Query, Path))

-- | Parse part of hash. Will return `Query (Map String String)` for query
-- | i.e. `"?foo=bar&bar=baz"` -->
-- |     `Query (fromList [Tuple "foo" "bar", Tuple "bar" "baz"])`
parsePart :: String -> RoutePart
parsePart str = fromMaybe (Path str) do
  guard $ S.take 1 str == "?"
  map (Query <<< M.fromList)
    $ traverse part2tuple parts
  where
  parts :: List String
  parts = toList $ S.split "&" $ S.drop 1 str

  part2tuple :: String -> Maybe (Tuple String String)
  part2tuple input = do
    let keyVal = S.split "=" input
    guard $ A.length keyVal <= 2
    Tuple <$> (A.head keyVal) <*> (keyVal A.!! 1)


-- | Parse hash string to `Route` with `decoder` function
-- | applied to every hash part (usually `decodeURIComponent`)
parse :: (String -> String) -> String -> Route
parse decoder hash =
  map ( decoder >>> parsePart ) $ toList (S.split "/" hash)
