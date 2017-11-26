module Routing
  ( RoutingEffects
  , foldHashes
  , hashes
  , matches
  , matchesWith
  , match
  , matchWith
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (hashchange)
import DOM.HTML.Types (windowToEventTarget)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Global (decodeURIComponent)
import Routing.Hash (getHash)
import Routing.Match (Match, runMatch)
import Routing.Parser (parse)

type RoutingEffects eff =
  ( dom :: DOM
  , ref :: REF
  | eff
  )

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
  let listener = eventListener \_ -> join (cb <$> readRef ref <*> getHash) >>= writeRef ref
  addEventListener hashchange listener false win
  pure $ removeEventListener hashchange listener false win

-- | Runs the callback on every hash change providing the previous hash and the
-- | latest hash. The initial hash is the empty string. The provided String is
-- | the hash portion of the `Location` with the '#' prefix stripped. Returns
-- | an effect which will remove the listener.
hashes
  :: forall eff
   . (String -> String -> Eff (RoutingEffects eff) Unit)
  -> Eff (RoutingEffects eff) (Eff (RoutingEffects eff) Unit)
hashes cb = foldHashes go (go "")
  where
  go a b = cb a b $> b

-- | Runs the callback on every hash change using a given `Match` parser to
-- | extract a route from the hash. If a hash fails to parse, it is ignored.
-- | To avoid dropping hashes, provide a fallback alternative in your parser.
-- | Returns an effect which will remove the listener.
matches
  :: forall eff a
   . Match a
  -> (Maybe a -> a -> Eff (RoutingEffects eff) Unit)
  -> Eff (RoutingEffects eff) (Eff (RoutingEffects eff) Unit)
matches = matchesWith decodeURIComponent

-- | Runs the callback on every hash change using a given custom String decoder
-- | and a `Match` parser to extract a route from the hash. If a hash fails to
-- | parse, it is ignored. To avoid dropping hashes, provide a fallback
-- | alternative in your parser. Returns an effect which will remove the
-- | listener.
matchesWith
  :: forall eff a
   . (String -> String)
  -> Match a
  -> (Maybe a -> a -> Eff (RoutingEffects eff) Unit)
  -> Eff (RoutingEffects eff) (Eff (RoutingEffects eff) Unit)
matchesWith decoder matcher cb = do
  let parser = matchWith decoder matcher
  foldHashes
    (\old -> either (\_ -> pure old) (\a -> cb old a $> Just a) <<< parser)
    (pure <<< either (const Nothing) Just <<< parser)

-- | Runs a `Match` parser.
match :: forall a. Match a -> String -> Either String a
match = matchWith decodeURIComponent

-- | Runs a `Match` parser given a custom String decoder.
matchWith :: forall a. (String -> String) -> Match a -> String -> Either String a
matchWith decoder matcher = runMatch matcher <<< parse decoder
