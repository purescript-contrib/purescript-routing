module Test.Browser where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Control.Monad.Except (runExcept)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Event.EventTypes (load)
import DOM.HTML.Types (HISTORY, htmlDocumentToDocument, htmlElementToNode, windowToEventTarget)
import DOM.HTML.Window (document)
import DOM.Node.Document (createElement, createTextNode)
import DOM.Node.Element (setAttribute)
import DOM.Node.Node (appendChild)
import DOM.Node.Types (Document, Node, elementToNode, textToNode)
import Data.Either (hush)
import Data.Foldable (oneOf)
import Data.Foreign (readInt, toForeign)
import Data.Maybe (Maybe(..), maybe)
import Data.Record as Rec
import Routing.Hash (RoutingEffects, hashes, setHash)
import Routing.Match (Match)
import Routing.Match.Class (lit)
import Routing.PushState (locations, makeInterface)

type Effects = RoutingEffects (exception :: EXCEPTION, history :: HISTORY)

data Route = A | B | U

route :: Match Route
route = oneOf
  [ lit "a" $> A
  , lit "b" $> B
  , pure U
  ]

type TestInterface =
  { assert :: String -> Boolean -> Eff Effects Unit
  , assertEq :: forall a. Show a => Eq a => String -> a -> a -> Eff Effects Unit
  }

withTest :: (TestInterface -> Eff Effects Unit) -> Eff Effects Unit
withTest k = do
  doc  <- window >>= document
  body <- body doc >>= maybe (throwException (error "Body not found")) pure

  let
    doc' :: Document
    doc' = htmlDocumentToDocument doc

    renderSuccess :: String -> Eff Effects Node
    renderSuccess testName = do
      row <- createElement "div" doc'
      setAttribute "class" "success" row
      tag <- createElement "b" doc'
      ok <- createTextNode "[OK]" doc'
      name <- createTextNode testName doc'
      _ <- appendChild (elementToNode tag) (elementToNode row)
      _ <- appendChild (textToNode name) (elementToNode row)
      _ <- appendChild (textToNode ok) (elementToNode tag)
      pure (elementToNode row)

    renderError :: String -> String -> Eff Effects Node
    renderError testName err = do
      row <- createElement "div" doc'
      setAttribute "class" "error" row
      tag <- createElement "b" doc'
      ok <- createTextNode "[FAIL]" doc'
      name <- createTextNode testName doc'
      error <- createElement "div" doc'
      setAttribute "class" "error-text" error
      errText <- createTextNode err doc'
      _ <- appendChild (textToNode ok) (elementToNode tag)
      _ <- appendChild (elementToNode tag) (elementToNode row)
      _ <- appendChild (textToNode name) (elementToNode row)
      _ <- appendChild (textToNode errText) (elementToNode error)
      _ <- appendChild (elementToNode error) (elementToNode row)
      pure (elementToNode row)

    assertEq :: forall a. Show a => Eq a => String -> a -> a -> Eff Effects Unit
    assertEq testName a b = do
      if a == b
        then do
          void $ flip appendChild (htmlElementToNode body) =<< renderSuccess testName
        else do
          let err = show a <> " /= " <> show b
          _ <- flip appendChild (htmlElementToNode body) =<< renderError testName err
          throwException (error $ testName <> ": " <> err)

    assert :: String -> Boolean -> Eff Effects Unit
    assert testName = assertEq testName true

  k { assert, assertEq }


runHashTests :: Eff Effects Unit -> Eff Effects Unit
runHashTests next = withTest \{ assert } -> do
  doneRef <- newRef (pure unit)
  let done = join (readRef doneRef) *> next
  writeRef doneRef =<< hashes case _, _ of
    Nothing, ""   -> assert "Hashes: Initial value" true
    Just "", "a"  -> assert "Hashes: ? -> a" true *> setHash "b"
    Just "a", "b" -> assert "Hashes: a -> b" true *> setHash ""
    Just "b", ""  -> assert "Hashes: b -> ?" true *> done
    _, _          -> assert "Hashes: fail" false
  setHash "a"

runPushStateTests :: Eff Effects Unit
runPushStateTests = withTest \{ assert } -> do
  hist <- makeInterface
  doneRef <- newRef (pure unit)
  let
    done = join (readRef doneRef)
    readState r = r { state = hush $ runExcept $ readInt r.state }
    loc1 = { state: Nothing, pathname: "/", search: "", hash: "", path: "/" }
    loc2 = { state: Just 1, pathname: "/a", search: "?a", hash: "", path: "/a?a" }
    loc3 = { state: Just 2, pathname: "/b", search: "", hash: "#b", path: "/b#b" }
    loc4 = { state: Just 3, pathname: "/c/d", search: "", hash: "", path: "/c/d" }
    loc5 = { state: Just 4, pathname: "/c/e", search: "", hash: "", path: "/c/e" }
    loc6 = { state: Just 5, pathname: "/c/e", search: "?f", hash: "", path: "/c/e?f" }
    loc7 = { state: Just 6, pathname: "/", search: "", hash: "", path: "/" }

  writeRef doneRef =<< flip locations hist \old new ->
    case readState <$> old, readState new of
      Nothing, new'
        | Rec.equal new' loc1 -> do
            assert "Locations: Initial" true
      Just old', new'
        | Rec.equal old' loc1 && Rec.equal new' loc2 -> do
            assert "Locations: init -> a" true
            hist.pushState (toForeign 2) "/b#b"
        | Rec.equal old' loc2 && Rec.equal new' loc3 -> do
            assert "Locations: a -> b" true
            hist.pushState (toForeign 3) "/c/d"
        | Rec.equal old' loc3 && Rec.equal new' loc4 -> do
            assert "Locations: b -> c/d" true
            hist.pushState (toForeign 4) "e"
        | Rec.equal old' loc4 && Rec.equal new' loc5 -> do
            assert "Locations: c/d -> c/e" true
            hist.pushState (toForeign 5) "?f"
        | Rec.equal old' loc5 && Rec.equal new' loc6 -> do
            assert "Locations: c/e -> c/e?f" true
            hist.pushState (toForeign 6) "/"
            done
      _, _ -> do
        done
        assert "Locations: fail" false
  hist.pushState (toForeign 1) "/a?a"

main :: Eff Effects Unit
main =
  window
    >>= windowToEventTarget
    >>> addEventListener load (eventListener (const run)) false

  where
  run = runHashTests runPushStateTests
