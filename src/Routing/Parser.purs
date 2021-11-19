module Routing.Parser (parse) where

import Prelude

import Routing.Types (Route, RoutePart(..))
import Data.Array as A
import Data.Map as M
import Data.String as S
import Control.MonadPlus (guard)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

-- | Parse query part of hash. Will return `Map String String` for query
-- | i.e. `"?foo=bar&bar=baz"` -->
-- |     `fromList [Tuple "foo" "bar", Tuple "bar" "baz"]`
parseQueryPart :: (String -> String) -> String -> Maybe (M.Map String String)
parseQueryPart decoder =
  map M.fromFoldable <<< traverse part2tuple <<< S.split (S.Pattern "&")
  where
  part2tuple :: String -> Maybe (Tuple String String)
  part2tuple input = do
    let keyVal = decoder <$> S.split (S.Pattern "=") input
    guard $ A.length keyVal <= 2
    Tuple <$> A.head keyVal <*> keyVal A.!! 1

-- | Parse hash string to `Route` with `decoder` function
-- | applied to every hash part (usually `decodeURIComponent`)
parse :: (String -> String) -> String -> Route
parse decoder hash =
  case flip S.splitAt hash <$> S.indexOf (S.Pattern "?") hash of
    Just { before, after } ->
      pathParts before
        <> map Query (L.fromFoldable (parseQueryPart decoder (S.drop 1 after)))
    Nothing ->
      pathParts hash
  where
  pathParts str = do
    let parts = L.fromFoldable $ map (Path <<< decoder) (S.split (S.Pattern "/") str)
    case L.unsnoc parts of
      Just { init, last: Path "" } -> init
      _ -> parts
