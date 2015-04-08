module Routing.Parser (
  parse
  ) where

import Control.MonadPlus
import Data.Maybe
import Data.Tuple
import Data.List
import Data.Traversable (traverse) 
import qualified Data.StrMap as M
import qualified Data.String as S
import qualified Data.Array as A

import Routing.Types


tryQuery :: RoutePart -> RoutePart
tryQuery source@(Path string) = fromMaybe source $ do
  guard $ S.take 1 string == "?"
  let parts = S.split "&" $ S.drop 1 string
  Query <$> M.fromList  <$> traverse part2tuple parts
  where part2tuple :: String -> Maybe (Tuple String String)
        part2tuple input = do
          let keyVal = S.split "=" input
          guard $ A.length keyVal <= 2
          Tuple <$> (A.head keyVal) <*> (keyVal A.!! 1)
tryQuery q = q


-- | Parse hash string to `Route` with `decoder` function
-- | applied to every hash part (usually `decodeURIComponent`)
parse :: (String -> String) -> String -> Route
parse decoder hash = tryQuery <$>
             Path <$>
             decoder <$> 
             fromArray (S.split "/" hash)

