module Test.Browser where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Effect.Ref as Ref
import Foreign (readInt, unsafeToForeign)
import Record as Rec
import Routing.Hash (hashes, setHash)
import Routing.Match (Match, lit)
import Routing.PushState (locations, makeInterface)
import Web.DOM.Document (createElement, createTextNode)
import Web.DOM.Document as Document
import Web.DOM.Element (setAttribute)
import Web.DOM.Element as Element
import Web.DOM.Node (Node)
import Web.DOM.Node as Node
import Web.DOM.Text as Text
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (load)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

data Route = A | B | U

route :: Match Route
route = oneOf
  [ lit "a" $> A
  , lit "b" $> B
  , pure U
  ]

type TestInterface =
  { assert :: String -> Boolean -> Effect Unit
  , assertEq :: forall a. Show a => Eq a => String -> a -> a -> Effect Unit
  }

withTest :: (TestInterface -> Effect Unit) -> Effect Unit
withTest k = do
  doc <- window >>= Window.document
  body <- HTMLDocument.body doc >>= maybe (throwException (error "Body not found")) pure

  let
    doc' :: Document.Document
    doc' = HTMLDocument.toDocument doc

    renderSuccess :: String -> Effect Node
    renderSuccess testName = do
      row <- createElement "div" doc'
      setAttribute "class" "success" row
      tag <- createElement "b" doc'
      ok <- createTextNode "[OK]" doc'
      name <- createTextNode testName doc'
      _ <- Node.appendChild (Element.toNode tag) (Element.toNode row)
      _ <- Node.appendChild (Text.toNode name) (Element.toNode row)
      _ <- Node.appendChild (Text.toNode ok) (Element.toNode tag)
      pure (Element.toNode row)

    renderError :: String -> String -> Effect Node
    renderError testName err = do
      row <- createElement "div" doc'
      setAttribute "class" "error" row
      tag <- createElement "b" doc'
      ok <- createTextNode "[FAIL]" doc'
      name <- createTextNode testName doc'
      error <- createElement "div" doc'
      setAttribute "class" "error-text" error
      errText <- createTextNode err doc'
      _ <- Node.appendChild (Text.toNode ok) (Element.toNode tag)
      _ <- Node.appendChild (Element.toNode tag) (Element.toNode row)
      _ <- Node.appendChild (Text.toNode name) (Element.toNode row)
      _ <- Node.appendChild (Text.toNode errText) (Element.toNode error)
      _ <- Node.appendChild (Element.toNode error) (Element.toNode row)
      pure (Element.toNode row)

    assertEq :: forall a. Show a => Eq a => String -> a -> a -> Effect Unit
    assertEq testName a b = do
      if a == b then do
        void $ flip Node.appendChild (HTMLElement.toNode body) =<< renderSuccess testName
      else do
        let err = show a <> " /= " <> show b
        _ <- flip Node.appendChild (HTMLElement.toNode body) =<< renderError testName err
        throwException (error $ testName <> ": " <> err)

    assert :: String -> Boolean -> Effect Unit
    assert testName = assertEq testName true

  k { assert, assertEq }

runHashTests :: Effect Unit -> Effect Unit
runHashTests next = withTest \{ assert } -> do
  doneRef <- Ref.new (pure unit)
  let done = join (Ref.read doneRef) *> next
  flip Ref.write doneRef =<< hashes case _, _ of
    Nothing, "" -> assert "Hashes: Initial value" true
    Just "", "a" -> assert "Hashes: ? -> a" true *> setHash "b"
    Just "a", "b" -> assert "Hashes: a -> b" true *> setHash ""
    Just "b", "" -> assert "Hashes: b -> ?" true *> done
    _, _ -> assert "Hashes: fail" false
  setHash "a"

runPushStateTests :: Effect Unit
runPushStateTests = withTest \{ assert } -> do
  hist <- makeInterface
  doneRef <- Ref.new (pure unit)
  let
    done = join (Ref.read doneRef)
    readState r = r { state = hush $ runExcept $ readInt r.state }
    loc1 = { state: Nothing, pathname: "/", search: "", hash: "", path: "/" }
    loc2 = { state: Just 1, pathname: "/a", search: "?a", hash: "", path: "/a?a" }
    loc3 = { state: Just 2, pathname: "/b", search: "", hash: "#b", path: "/b#b" }
    loc4 = { state: Just 3, pathname: "/c/d", search: "", hash: "", path: "/c/d" }
    loc5 = { state: Just 4, pathname: "/c/e", search: "", hash: "", path: "/c/e" }
    loc6 = { state: Just 5, pathname: "/c/e", search: "?f", hash: "", path: "/c/e?f" }
    loc7 = { state: Just 6, pathname: "/", search: "", hash: "", path: "/" }

  flip Ref.write doneRef =<< flip locations hist \old new ->
    case readState <$> old, readState new of
      Nothing, new'
        | Rec.equal new' loc1 -> do
            assert "Locations: Initial" true
      Just old', new'
        | Rec.equal old' loc1 && Rec.equal new' loc2 -> do
            assert "Locations: init -> a" true
            hist.pushState (unsafeToForeign 2) "/b#b"
        | Rec.equal old' loc2 && Rec.equal new' loc3 -> do
            assert "Locations: a -> b" true
            hist.pushState (unsafeToForeign 3) "/c/d"
        | Rec.equal old' loc3 && Rec.equal new' loc4 -> do
            assert "Locations: b -> c/d" true
            hist.pushState (unsafeToForeign 4) "e"
        | Rec.equal old' loc4 && Rec.equal new' loc5 -> do
            assert "Locations: c/d -> c/e" true
            hist.pushState (unsafeToForeign 5) "?f"
        | Rec.equal old' loc5 && Rec.equal new' loc6 -> do
            assert "Locations: c/e -> c/e?f" true
            hist.pushState (unsafeToForeign 6) "/"
        | Rec.equal old' loc6 && Rec.equal new' loc7 -> do
            assert "Locations: c/e?f -> /" true
            done
      _, _ -> do
        done
        assert "Locations: fail" false
  hist.pushState (unsafeToForeign 1) "/a?a"

main :: Effect Unit
main = do
  listener <- eventListener \_ -> runHashTests runPushStateTests
  window
    >>= Window.toEventTarget
      >>> addEventListener load listener false
