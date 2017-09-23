module Routing (
  hashChanged,
  hashes,
  matches,
  matches',
  match,
  matchWith,
  matchesAff,
  matchesAff'
  ) where

import Control.Monad.Aff (Aff, makeAff, nonCanceler)
import Control.Monad.Eff (Eff)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.String.Regex as R
import Data.String.Regex.Flags as RF
import Data.Tuple (Tuple(..))
import Prelude (Unit, const, pure, unit, ($), (<$))
import Routing.Match (Match, runMatch)
import Routing.Parser (parse)


foreign import decodeURIComponent :: String -> String

foreign import hashChanged :: forall e. (String -> String -> Eff e Unit) -> Eff e Unit


hashes :: forall e. (String -> String -> Eff e Unit) -> Eff e Unit
hashes cb =
  hashChanged $ \old new -> do
    cb (dropHash old) (dropHash new)
  where dropHash h =
          case R.regex "^[^#]*#" RF.noFlags of
            Right regX -> R.replace regX "" h
            Left _     -> h


-- | Stream of hash changed, callback called when new hash can be matched
-- | First argument of callback is `Just a` when old hash can be matched
-- | and `Nothing` when it can't.
matches :: forall e a. Match a -> (Maybe a -> a -> Eff e Unit) -> Eff e Unit
matches = matches' decodeURIComponent

matches' :: forall e a. (String -> String) ->
            Match a -> (Maybe a -> a -> Eff e Unit) -> Eff e Unit
matches' decoder routing cb = hashes $ \old new ->
  let mr = matchWith decoder routing
      fst = either (const Nothing) Just $ mr old
  in either (const $ pure unit) (cb fst) $ mr new

matchesAff' :: forall e a. (String -> String) ->
               Match a -> Aff e (Tuple (Maybe a) a)
matchesAff' decoder routing =
  makeAff \k -> nonCanceler <$
    matches' decoder routing \old new ->
      k $ Right $ Tuple old new

matchesAff :: forall e a. Match a -> Aff e (Tuple (Maybe a) a)
matchesAff = matchesAff' decodeURIComponent


match :: forall a. Match a -> String -> Either String a
match = matchWith decodeURIComponent

matchWith :: forall a. (String -> String) -> Match a -> String -> Either String a
matchWith decoder matcher hash = runMatch matcher $ parse decoder hash
