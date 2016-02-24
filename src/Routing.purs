module Routing (
  hashChanged,
  hashes,
  matches,
  matches',
  matchHash,
  matchHash',
  matchesAff,
  matchesAff'
  ) where

import Prelude (Unit, ($), unit, pure, const)
import Control.Monad.Eff (Eff())
import Control.Monad.Aff (Aff(), makeAff)
import Data.Maybe (Maybe(..))
import Data.Either (Either(), either)
import Data.Tuple (Tuple(..))
import Data.String.Regex as R

import Routing.Parser (parse)
import Routing.Match (Match, runMatch)


foreign import decodeURIComponent :: String -> String

foreign import hashChanged :: forall e. (String -> String -> Eff e Unit) -> Eff e Unit


hashes :: forall e. (String -> String -> Eff e Unit) -> Eff e Unit
hashes cb =
  hashChanged $ \old new -> do
    cb (dropHash old) (dropHash new)
  where dropHash h = R.replace (R.regex "^[^#]*#" R.noFlags) "" h


-- | Stream of hash changed, callback called when new hash can be matched
-- | First argument of callback is `Just a` when old hash can be matched
-- | and `Nothing` when it can't.
matches :: forall e a. Match a -> (Maybe a -> a -> Eff e Unit) -> Eff e Unit
matches = matches' decodeURIComponent

matches' :: forall e a. (String -> String) ->
            Match a -> (Maybe a -> a -> Eff e Unit) -> Eff e Unit
matches' decoder routing cb = hashes $ \old new ->
  let mr = matchHash' decoder routing
      fst = either (const Nothing) Just $ mr old
  in either (const $ pure unit) (cb fst) $ mr new

matchesAff' :: forall e a. (String -> String) ->
               Match a -> Aff e (Tuple (Maybe a) a)
matchesAff' decoder routing =
  makeAff \_ k -> do
    matches' decoder routing \old new ->
      k $ Tuple old new

matchesAff :: forall e a. Match a -> Aff e (Tuple (Maybe a) a)
matchesAff = matchesAff' decodeURIComponent


matchHash :: forall a. Match a -> String -> Either String a
matchHash = matchHash' decodeURIComponent

matchHash' :: forall a. (String -> String) -> Match a -> String -> Either String a
matchHash' decoder matcher hash = runMatch matcher $ parse decoder hash
