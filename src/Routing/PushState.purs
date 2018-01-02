module Routing.PushState
  ( PushStateEffects
  , PushStateInterface
  , LocationState
  , Basename(..)
  , Path(..)
  , makeInterface
  , foldLocations
  , locations
  , foldPaths
  , paths
  , matches
  , matchesWith
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef, writeRef)
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
import Data.Array (find)
import Data.Array as Array
import Data.Foldable (class Foldable, for_, traverse_)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap as StrMap
import Data.String as String
import Routing (match)
import Routing.Match (Match)

type PushStateEffects eff =
  ( history :: HISTORY
  , dom :: DOM
  , ref :: REF
  | eff
  )

newtype Basename = Basename String

derive newtype instance eqBasename :: Eq Basename
derive newtype instance ordBasename :: Ord Basename
derive instance newtypeBasename :: Newtype Basename _

newtype Path = Path String

derive newtype instance eqPath :: Eq Path
derive newtype instance ordPath :: Ord Path
derive instance newtypePath :: Newtype Path _

type PushStateInterface eff =
  { pushState :: Foreign -> Path -> Eff eff Unit
  , replaceState :: Foreign -> Path -> Eff eff Unit
  , locationState :: Eff eff LocationState
  , listen :: (LocationState -> Eff eff Unit) -> Eff eff (Eff eff Unit)
  }

makeInterface :: forall eff. Basename -> Eff (PushStateEffects eff) (PushStateInterface (PushStateEffects eff))
makeInterface basename = do
  argsRef <- newRef []
  freshRef <- newRef 0
  listenersRef <- newRef StrMap.empty

  let
    notify ev =
      traverse_ (_ $ ev) =<< readRef listenersRef

    listen k = do
      fresh <- readRef freshRef
      writeRef freshRef (fresh + 1)
      modifyRef listenersRef $ StrMap.insert (show fresh) k
      pure $ modifyRef listenersRef $ StrMap.delete (show fresh)

    locationState = do
      loc <- DOM.window >>= DOM.location
      state <- DOM.window >>= DOM.history >>= DOM.state
      pathname <- fromMaybe <*> String.stripPrefix (String.Pattern (unwrap basename)) <$> DOM.pathname loc
      search <- DOM.search loc
      hash <- DOM.hash loc
      pure { state, pathname, search, hash }

  schedule <- do
    obsvNode <-
      DOM.window
        >>= DOM.document
        >>> map DOM.htmlDocumentToDocument
        >>= DOM.createTextNode ""
        >>> map DOM.textToNode
    observer <- DOM.mutationObserver \_ _ -> do
      argsQueue <- readRef argsRef
      writeRef argsRef []
      for_ argsQueue notify
    DOM.observe obsvNode { characterData: true } observer
    pure \args -> do
      argsQueue <- readRef argsRef
      writeRef argsRef (Array.snoc argsQueue args)
      when (Array.null argsQueue) do
        fresh ← readRef freshRef
        writeRef freshRef (fresh + 1)
        DOM.setNodeValue (show fresh) obsvNode

  let
    stateFn op state path = do
      let
        loc = normalizeLocation basename state path
        url = DOM.URL $ loc.pathname <> loc.search <> loc.hash
      DOM.window
        >>= DOM.history
        >>= op state (DOM.DocumentTitle "") url
      schedule loc

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

type LocationState =
  { state :: Foreign
  , pathname :: String
  , search :: String
  , hash :: String
  }

normalizeLocation :: Basename -> Foreign -> Path -> LocationState
normalizeLocation (Basename basename) state (Path path) =
  case searchIx, hashIx of
    Nothing, Nothing   -> { state, pathname: path', hash: "", search: "" }
    Nothing, Just hix  -> { state, pathname: String.take hix path', search: "", hash: String.drop hix path' }
    Just six, Nothing  -> { state, pathname: String.take six path', search: String.drop six path', hash: "" }
    Just six, Just hix -> { state, pathname: String.take hix path', search: String.take (hix - six) (String.drop six path'), hash: String.drop hix path' }
  where
  path' = basename <> path
  searchIx = String.indexOf (String.Pattern "?") path'
  hashIx = String.indexOf (String.Pattern "#") path'

foldLocations
  :: forall eff a
   . (a -> LocationState -> Eff (PushStateEffects eff) a)
  -> (LocationState -> Eff (PushStateEffects eff) a)
  -> PushStateInterface (PushStateEffects eff)
  -> Eff (PushStateEffects eff) (Eff (PushStateEffects eff) Unit)
foldLocations cb init psi = do
  ref <- newRef =<< init =<< psi.locationState
  psi.listen (\loc -> writeRef ref =<< flip cb loc =<< readRef ref)

locations
  :: forall eff a
   . (Maybe LocationState -> LocationState -> Eff (PushStateEffects eff) Unit)
  -> PushStateInterface (PushStateEffects eff)
  -> Eff (PushStateEffects eff) (Eff (PushStateEffects eff) Unit)
locations cb = foldLocations go (go Nothing)
  where
  go a b = Just b <$ cb a b

foldPaths
  :: forall eff a
   . (a -> String -> Eff (PushStateEffects eff) a)
  -> (String -> Eff (PushStateEffects eff) a)
  -> PushStateInterface (PushStateEffects eff)
  -> Eff (PushStateEffects eff) (Eff (PushStateEffects eff) Unit)
foldPaths cb init = foldLocations (\a -> cb a <<< toString) (init <<< toString)
  where
  toString loc = loc.pathname <> loc.search <> loc.hash

paths
  :: forall eff
   . (Maybe String -> String -> Eff (PushStateEffects eff) Unit)
  -> PushStateInterface (PushStateEffects eff)
  -> Eff (PushStateEffects eff) (Eff (PushStateEffects eff) Unit)
paths = matchesWith Just

matches
  :: forall eff a
   . Match a
  -> (Maybe a -> a -> Eff (PushStateEffects eff) Unit)
  -> PushStateInterface (PushStateEffects eff)
  -> Eff (PushStateEffects eff) (Eff (PushStateEffects eff) Unit)
matches = matchesWith <<< match

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
      <<< find (const true)
      <<< parser
