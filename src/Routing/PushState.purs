module Routing.PushState
  ( PushStateInterface
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

import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, indexl, traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Ref as Ref
import Foreign (Foreign)
import Routing (match)
import Routing.Match (Match)
import Web.DOM.Document (createTextNode) as DOM
import Web.DOM.MutationObserver (mutationObserver, observe) as DOM
import Web.DOM.Node (setNodeValue) as DOM
import Web.DOM.Text as Text
import Web.Event.EventTarget (addEventListener, eventListener) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Event.PopStateEvent.EventTypes as ET
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.History as History
import Web.HTML.Location (hash, pathname, search) as DOM
import Web.HTML.Window as Window

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
type PushStateInterface =
  { pushState :: Foreign -> String -> Effect Unit
  , replaceState :: Foreign -> String -> Effect Unit
  , locationState :: Effect LocationState
  , listen :: (LocationState -> Effect Unit) -> Effect (Effect Unit)
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
makeInterface :: Effect (PushStateInterface)
makeInterface = do
  freshRef <- Ref.new 0
  listenersRef <- Ref.new Map.empty

  let
    notify ev =
      traverse_ (_ $ ev) =<< Ref.read listenersRef

    listen k = do
      fresh <- Ref.read freshRef
      Ref.write (fresh + 1) freshRef
      Ref.modify_ (Map.insert fresh k) listenersRef
      pure $ Ref.modify_ (Map.delete fresh) listenersRef

    locationState = do
      loc <- DOM.window >>= Window.location
      state <- DOM.window >>= Window.history >>= History.state
      pathname <- DOM.pathname loc
      search <- DOM.search loc
      hash <- DOM.hash loc
      let path = pathname <> search <> hash
      pure { state, pathname, search, hash, path }

  -- The hashchange interface is asynchronous, since hashchange events are
  -- fired on the next tick of the event loop. We want the push-state
  -- interface to behave as similarly as possible, so we use `makeImmediate`
  -- which will execute `notify` maximum once per event loop.
  schedule <- makeImmediate $ notify =<< locationState

  let
    stateFn op state path = do
      DOM.window
        >>= Window.history
        >>= op state (History.DocumentTitle "") (History.URL path)
      schedule

  listener <- DOM.eventListener \_ -> notify =<< locationState

  DOM.window
    >>= Window.toEventTarget
      >>> DOM.addEventListener ET.popstate listener false

  pure
    { pushState: stateFn History.pushState
    , replaceState: stateFn History.replaceState
    , locationState
    , listen
    }

-- | Folds effectfully over location changes given callbacks for handling
-- | changes and the initial location. Returns an effect which removes the
-- | listener.
foldLocations
  :: forall a
   . (a -> LocationState -> Effect a)
  -> (LocationState -> Effect a)
  -> PushStateInterface
  -> Effect (Effect Unit)
foldLocations cb init psi = do
  ref <- Ref.new =<< init =<< psi.locationState
  psi.listen (\loc -> flip Ref.write ref =<< flip cb loc =<< Ref.read ref)

-- | Runs the callback on every location change providing the previous location
-- | and the latest location. Returns an effect which removes the listener.
locations
  :: (Maybe LocationState -> LocationState -> Effect Unit)
  -> PushStateInterface
  -> Effect (Effect Unit)
locations cb = foldLocations go (go Nothing)
  where
  go a b = Just b <$ cb a b

-- | Folds effectfully over path changes given callbacks for handling changes
-- | and the initial path. Returns an effect which removes the listener.
foldPaths
  :: forall a
   . (a -> String -> Effect a)
  -> (String -> Effect a)
  -> PushStateInterface
  -> Effect (Effect Unit)
foldPaths cb init = foldLocations (\a -> cb a <<< _.path) (init <<< _.path)

-- | Runs the callback on every path change providing the previous path and
-- | the latest path. Returns an effect which removes the listener.
paths
  :: (Maybe String -> String -> Effect Unit)
  -> PushStateInterface
  -> Effect (Effect Unit)
paths = matchesWith Just

-- | Runs the callback on every path change using a given `Match` parser to
-- | extract a route from the path. If a path fails to parse, it is ignored.
-- | To avoid dropping paths, provide a fallback alternative in your parser.
-- | Returns an effect which removes the listener.
matches
  :: forall a
   . Match a
  -> (Maybe a -> a -> Effect Unit)
  -> PushStateInterface
  -> Effect (Effect Unit)
matches = matchesWith <<< match

-- | Runs the callback on every path change using a given custom parser to
-- | extract a route from the path. If a path fails to parse, it is ignored.
-- | To avoid dropping paths, provide a fallback alternative in your parser.
-- | Returns an effect which removes the listener.
matchesWith
  :: forall f a
   . Foldable f
  => (String -> f a)
  -> (Maybe a -> a -> Effect Unit)
  -> PushStateInterface
  -> Effect (Effect Unit)
matchesWith parser cb = foldPaths go (go Nothing)
  where
  go a =
    maybe (pure a) (\b -> Just b <$ cb a b)
      <<< indexl 0
      <<< parser

-- | Similar to `setImmediate`, it's implemented using microtask queue via MutationObserver
-- | to schedule callbacks. This way it's more immediate than `setTimout` would have been.
-- | We use a fresh counter so that the text change mutation always fires.
-- | from: https://github.com/natefaubion/purescript-spork/blob/3b56c4d36e84866ed9b1bc27afa7ab4762ffdd01/src/Spork/Scheduler.purs#L20
makeImmediate :: Effect Unit -> Effect (Effect Unit)
makeImmediate run = do
  document <-
    DOM.window
      >>= Window.document
        >>> map HTMLDocument.toDocument
  nextTick <- Ref.new (Right 0)
  obsvNode <- Text.toNode <$> DOM.createTextNode "" document
  observer <- DOM.mutationObserver \_ _ -> do
    Ref.modify_ (either (Right <<< add 1) Right) nextTick
    run
  DOM.observe obsvNode { characterData: true } observer
  pure do
    Ref.read nextTick >>= traverse_ \tick -> do
      Ref.write (Left (tick + 1)) nextTick
      DOM.setNodeValue (show tick) obsvNode
