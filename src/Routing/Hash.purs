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

import Data.Foldable (class Foldable, indexl)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), stripPrefix)
import Effect (Effect)
import Effect.Ref as Ref
import Routing (match, matchWith)
import Routing.Match (Match)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Event.HashChangeEvent.EventTypes as ET
import Web.HTML.Location as L
import Web.HTML.Window as Window

-- | Gets the global location hash.
getHash :: Effect String
getHash = window >>= Window.location >>= L.hash >>> map (stripPrefix (Pattern "#") >>> fromMaybe "")

-- | Sets the global location hash.
setHash :: String -> Effect Unit
setHash h = window >>= Window.location >>= L.setHash h

-- | Modifies the global location hash.
modifyHash :: (String -> String) -> Effect Unit
modifyHash fn = (fn <$> getHash) >>= setHash

-- | Folds effectfully over hash changes given a callback and an initial hash.
-- | The provided String is the hash portion of the `Location` with the '#'
-- | prefix stripped. Returns an effect which will remove the listener.
foldHashes
  :: forall a
   . (a -> String -> Effect a)
  -> (String -> Effect a)
  -> Effect (Effect Unit)
foldHashes cb init = do
  ref <- Ref.new =<< init =<< getHash
  win <- Window.toEventTarget <$> window
  listener <- eventListener \_ -> flip Ref.write ref =<< join (cb <$> Ref.read ref <*> getHash)
  addEventListener ET.hashchange listener false win
  pure $ removeEventListener ET.hashchange listener false win

-- | Runs the callback on every hash change providing the previous hash and the
-- | latest hash. The provided String is the hash portion of the `Location` with
-- | the '#' prefix stripped. Returns an effect which will remove the listener.
hashes :: (Maybe String -> String -> Effect Unit) -> Effect (Effect Unit)
hashes = matchesWith Just

-- | Runs the callback on every hash change using a given `Match` parser to
-- | extract a route from the hash. If a hash fails to parse, it is ignored.
-- | To avoid dropping hashes, provide a fallback alternative in your parser.
-- | Returns an effect which will remove the listener.
matches
  :: forall a
   . Match a
  -> (Maybe a -> a -> Effect Unit)
  -> Effect (Effect Unit)
matches = matchesWith <<< match

-- | Runs the callback on every hash change using a given custom parser to
-- | extract a route from the hash. If a hash fails to parse, it is ignored.
-- | To avoid dropping hashes, provide a fallback alternative in your parser.
-- | Returns an effect which will remove the listener.
matchesWith
  :: forall f a
   . Foldable f
  => (String -> f a)
  -> (Maybe a -> a -> Effect Unit)
  -> Effect (Effect Unit)
matchesWith parser cb = foldHashes go (go Nothing)
  where
  go a =
    maybe (pure a) (\b -> Just b <$ cb a b)
      <<< indexl 0
      <<< parser
