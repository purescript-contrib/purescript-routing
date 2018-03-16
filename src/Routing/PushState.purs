module Routing.PushState
  ( PushStateEffects
  , PushStateInterface
  , LocationState
  , makeInterface
  , foldLocations
  , locations
  , foldPaths
  , paths
  , matches
  , matchesWith
  ) where

import Prelude

import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef, writeRef)
import Control.Monad.Eff.Ref.Unsafe (unsafeRunRef)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Event.EventTypes (popstate) as DOM
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState, replaceState, state) as DOM
import DOM.HTML.Location (hash, pathname, search) as DOM
import DOM.HTML.Types (HISTORY)
import DOM.HTML.Types (htmlDocumentToDocument, windowToEventTarget) as DOM
import DOM.HTML.Window (document, history, location) as DOM
import DOM.Node.Document (createTextNode) as DOM
import DOM.Node.MutationObserver (mutationObserver, observe) as DOM
import DOM.Node.Node (setNodeValue) as DOM
import DOM.Node.Types (textToNode) as DOM
import Data.Foldable (class Foldable, indexl, sequence_, traverse_)
import Data.Foreign (Foreign)
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Routing (match)
import Routing.Match (Match)

type PushStateEffects eff =
  ( history :: HISTORY
  , dom :: DOM
  , ref :: REF
  | eff
  )

-- | A `PushStateInterface` is a localized instance for making location changes
-- | and consuming the events. Since the DOM API does not provide a general
-- | event type for push state (only pop), you must use the coupled effects
-- | along with `listen` to receive events on all location changes.
-- |
-- | * `pushState` – pushes a new location state and path onto the history stack.
-- | * `replaceState` – replaces the location state and path in the history stack.
-- | * `locationState` – Dereferences the current history state
-- | * `listen` – Subscribes to location changes (both push and pop). Returns
-- |   an effect which removes the listener.
type PushStateInterface eff =
  { pushState :: Foreign -> String -> Eff eff Unit
  , replaceState :: Foreign -> String -> Eff eff Unit
  , locationState :: Eff eff LocationState
  , listen :: (LocationState -> Eff eff Unit) -> Eff eff (Eff eff Unit)
  }

type LocationState =
  { state :: Foreign
  , path :: String
  , pathname :: String
  , search :: String
  , hash :: String
  }

-- | Creates a new `PushStateInterface`. Generally you should only create one
-- | instance for your application. Since the DOM does not provide general
-- | events for location changes, listeners will only be notified on push when
-- | using the paired functions.
makeInterface :: forall eff. Eff (PushStateEffects eff) (PushStateInterface (PushStateEffects eff))
makeInterface = do
  schedRef <- newRef false
  freshRef <- newRef 0
  listenersRef <- newRef Map.empty

  let
    notify ev =
      traverse_ (_ $ ev) =<< readRef listenersRef

    listen k = do
      fresh <- readRef freshRef
      writeRef freshRef (fresh + 1)
      modifyRef listenersRef $ Map.insert fresh k
      pure $ modifyRef listenersRef $ Map.delete fresh

    locationState = do
      loc <- DOM.window >>= DOM.location
      state <- DOM.window >>= DOM.history >>= DOM.state
      pathname <- DOM.pathname loc
      search <- DOM.search loc
      hash <- DOM.hash loc
      let path = pathname <> search <> hash
      pure { state, pathname, search, hash, path }

  let
    stateFn op state path = do
      DOM.window
        >>= DOM.history
        >>= op state (DOM.DocumentTitle "") (DOM.URL path)
      -- The hashchange interface is asynchronous, since hashchange events are
      -- fired on the next tick of the event loop. We want the push-state
      -- interface to behave as similarly as possible, so we use something like
      -- `setImmediate` and use `Ref Boolean` to make sure maximum one `notify` is
      -- scheduled per event loop.
      unlessM (readRef schedRef) do
        writeRef schedRef true
        unsafeSetImmediate $ do
          writeRef schedRef false
          notify =<< locationState

    listener =
      DOM.eventListener \_ -> notify =<< locationState

  DOM.window
    >>= DOM.windowToEventTarget
    >>> DOM.addEventListener DOM.popstate listener false

  pure
    { pushState: stateFn DOM.pushState
    , replaceState: stateFn DOM.replaceState
    , locationState
    , listen
    }

-- | Folds effectfully over location changes given callbacks for handling
-- | changes and the initial location. Returns an effect which removes the
-- | listener.
foldLocations
  :: forall eff a
   . (a -> LocationState -> Eff (PushStateEffects eff) a)
  -> (LocationState -> Eff (PushStateEffects eff) a)
  -> PushStateInterface (PushStateEffects eff)
  -> Eff (PushStateEffects eff) (Eff (PushStateEffects eff) Unit)
foldLocations cb init psi = do
  ref <- newRef =<< init =<< psi.locationState
  psi.listen (\loc -> writeRef ref =<< flip cb loc =<< readRef ref)

-- | Runs the callback on every location change providing the previous location
-- | and the latest location. Returns an effect which removes the listener.
locations
  :: forall eff
   . (Maybe LocationState -> LocationState -> Eff (PushStateEffects eff) Unit)
  -> PushStateInterface (PushStateEffects eff)
  -> Eff (PushStateEffects eff) (Eff (PushStateEffects eff) Unit)
locations cb = foldLocations go (go Nothing)
  where
  go a b = Just b <$ cb a b

-- | Folds effectfully over path changes given callbacks for handling changes
-- | and the initial path. Returns an effect which removes the listener.
foldPaths
  :: forall eff a
   . (a -> String -> Eff (PushStateEffects eff) a)
  -> (String -> Eff (PushStateEffects eff) a)
  -> PushStateInterface (PushStateEffects eff)
  -> Eff (PushStateEffects eff) (Eff (PushStateEffects eff) Unit)
foldPaths cb init = foldLocations (\a -> cb a <<< _.path) (init <<< _.path)

-- | Runs the callback on every path change providing the previous path and
-- | the latest path. Returns an effect which removes the listener.
paths
  :: forall eff
   . (Maybe String -> String -> Eff (PushStateEffects eff) Unit)
  -> PushStateInterface (PushStateEffects eff)
  -> Eff (PushStateEffects eff) (Eff (PushStateEffects eff) Unit)
paths = matchesWith Just

-- | Runs the callback on every path change using a given `Match` parser to
-- | extract a route from the path. If a path fails to parse, it is ignored.
-- | To avoid dropping paths, provide a fallback alternative in your parser.
-- | Returns an effect which removes the listener.
matches
  :: forall eff a
   . Match a
  -> (Maybe a -> a -> Eff (PushStateEffects eff) Unit)
  -> PushStateInterface (PushStateEffects eff)
  -> Eff (PushStateEffects eff) (Eff (PushStateEffects eff) Unit)
matches = matchesWith <<< match

-- | Runs the callback on every path change using a given custom parser to
-- | extract a route from the path. If a path fails to parse, it is ignored.
-- | To avoid dropping paths, provide a fallback alternative in your parser.
-- | Returns an effect which removes the listener.
matchesWith
  :: forall eff f a
   . Foldable f
  => (String -> f a)
  -> (Maybe a -> a -> Eff (PushStateEffects eff) Unit)
  -> PushStateInterface (PushStateEffects eff)
  -> Eff (PushStateEffects eff) (Eff (PushStateEffects eff) Unit)
matchesWith parser cb = foldPaths go (go Nothing)
  where
  go a =
    maybe (pure a) (\b -> Just b <$ cb a b)
      <<< indexl 0
      <<< parser

-- | Similar to `setImmediate`, it's impelemnted using microtask queue via MutationObserver
-- | to schedule callbacks. this way it's more imediate then `setTimout` would have been.
-- | We use a fresh counter so that the text change mutation always fires.
-- |
-- | NOTE: it's not that unsafe, but we use `unsafeRunRef` and `runPure` so it's still unsafe.
unsafeSetImmediate
  :: forall r
  . Eff (ref :: REF, dom :: DOM |r) Unit -> Eff (ref :: REF, dom :: DOM |r) Unit
unsafeSetImmediate =
  let
    freshRef = runPure $ unsafeRunRef $ newRef 0
    effRef = runPure $ unsafeRunRef $ newRef Nil
    nodeRef = runPure $ unsafeRunRef $ newRef Nothing
  in \eff -> do
    modifyRef effRef $ Cons eff
    obsvNode <- readRef nodeRef >>= case _ of
      Nothing -> do
        obsvNode <-
          DOM.window
            >>= DOM.document
            >>> map DOM.htmlDocumentToDocument
            >>= DOM.createTextNode ""
            >>> map DOM.textToNode
        observer <- DOM.mutationObserver \_ _ -> do
          readRef effRef >>= sequence_
          writeRef effRef Nil
        DOM.observe obsvNode { characterData: true } observer
        pure obsvNode
      Just obsvNode ->
        pure obsvNode
    fresh ← readRef freshRef
    writeRef freshRef (fresh + 1)
    DOM.setNodeValue (show fresh) obsvNode

