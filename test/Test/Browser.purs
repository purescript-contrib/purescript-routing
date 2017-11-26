module Test.Browser where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Event.EventTypes (load)
import DOM.HTML.Types (htmlDocumentToDocument, htmlElementToNode, windowToEventTarget)
import DOM.HTML.Window (document)
import DOM.Node.Document (createElement, createTextNode)
import DOM.Node.Element (setAttribute)
import DOM.Node.Node (appendChild)
import DOM.Node.Types (Document, Node, elementToNode, textToNode)
import Data.Foldable (oneOf)
import Data.Maybe (maybe)
import Routing (RoutingEffects, hashes)
import Routing.Hash (setHash)
import Routing.Match (Match)
import Routing.Match.Class (lit)

type Effects = RoutingEffects (exception :: EXCEPTION)

data Route = A | B | U

route :: Match Route
route = oneOf
  [ lit "a" $> A
  , lit "b" $> B
  , pure U
  ]

runTests :: Eff Effects Unit
runTests = do
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

  _ <- hashes case _, _ of
    "", ""   -> assert "Hashes: Initial value" true
    "", "a"  -> assert "Hashes: ? -> a" true *> setHash "b"
    "a", "b" -> assert "Hashes: a -> b" true *> setHash ""
    "b", ""  -> assert "Hashes: b -> ?" true
    _, _     -> assert "Hashes: fail" false

  setHash "a"

main :: Eff Effects Unit
main =
  window
    >>= windowToEventTarget
    >>> addEventListener load (eventListener (const runTests)) false
