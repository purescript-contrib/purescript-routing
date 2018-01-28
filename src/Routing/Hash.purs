module Routing.Hash
  ( getHash
  , setHash
  , modifyHash
  , foldHashes
  , hashes
  , matches
  , matchesWith
  , module Routing
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (hashchange)
import DOM.HTML.Location as L
import DOM.HTML.Types (windowToEventTarget)
import DOM.HTML.Window (location)
import Data.Foldable (class Foldable, indexl)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), stripPrefix)
import Routing (RoutingEffects, match, matchWith)
import Routing.Match (Match)

-- | Gets the global location hash.
getHash :: forall eff. Eff (dom :: DOM | eff) String
getHash = window >>= location >>= L.hash >>> map (stripPrefix (Pattern "#") >>> fromMaybe "")

-- | Sets the global location hash.
setHash :: forall eff. String -> Eff (dom :: DOM | eff) Unit
setHash h = window >>= location >>= L.setHash h

-- | Modifies the global location hash.
modifyHash :: forall eff. (String -> String) -> Eff (dom :: DOM | eff) Unit
modifyHash fn = (fn <$> getHash) >>= setHash

-- | Folds effectfully over hash changes given a callback and an initial hash.
-- | The provided String is the hash portion of the `Location` with the '#'
-- | prefix stripped. Returns an effect which will remove the listener.
foldHashes
  :: forall eff a
   . (a -> String -> Eff (RoutingEffects eff) a)
  -> (String -> Eff (RoutingEffects eff) a)
  -> Eff (RoutingEffects eff) (Eff (RoutingEffects eff) Unit)
foldHashes cb init = do
  ref <- newRef =<< init =<< getHash
  win <- windowToEventTarget <$> window
  let listener = eventListener \_ -> writeRef ref =<< join (cb <$> readRef ref <*> getHash)
  addEventListener hashchange listener false win
  pure $ removeEventListener hashchange listener false win

-- | Runs the callback on every hash change providing the previous hash and the
-- | latest hash. The provided String is the hash portion of the `Location` with
-- | the '#' prefix stripped. Returns an effect which will remove the listener.
hashes
  :: forall eff
   . (Maybe String -> String -> Eff (RoutingEffects eff) Unit)
  -> Eff (RoutingEffects eff) (Eff (RoutingEffects eff) Unit)
hashes = matchesWith Just

-- | Runs the callback on every hash change using a given `Match` parser to
-- | extract a route from the hash. If a hash fails to parse, it is ignored.
-- | To avoid dropping hashes, provide a fallback alternative in your parser.
-- | Returns an effect which will remove the listener.
matches
  :: forall eff a
   . Match a
  -> (Maybe a -> a -> Eff (RoutingEffects eff) Unit)
  -> Eff (RoutingEffects eff) (Eff (RoutingEffects eff) Unit)
matches = matchesWith <<< match

-- | Runs the callback on every hash change using a given custom parser to
-- | extract a route from the hash. If a hash fails to parse, it is ignored.
-- | To avoid dropping hashes, provide a fallback alternative in your parser.
-- | Returns an effect which will remove the listener.
matchesWith
  :: forall eff f a
   . Foldable f
  => (String -> f a)
  -> (Maybe a -> a -> Eff (RoutingEffects eff) Unit)
  -> Eff (RoutingEffects eff) (Eff (RoutingEffects eff) Unit)
matchesWith parser cb = foldHashes go (go Nothing)
  where
  go a =
    maybe (pure a) (\b -> Just b <$ cb a b)
      <<< indexl 0
      <<< parser
