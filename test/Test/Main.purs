module Test.Main where

import Control.Monad.Eff
import Data.Tuple
import Data.Array
import Data.Either
import Data.Maybe
import Data.StrMap (lookup)
import Global

import Debug.Trace
import Debug.Foreign

import Routing.Setter
import Routing.Getter

routing :: PErr Route
routing = do
  one <- route "notebook" "notebook?foo&bar"
  two <- route "file" "file/:id"
  three <- route "read" "/read"
  four <- route "write" "/write"
  pure $ one `or` (two `contains` (three `or` four))
  
data Test =
  Notebook Number Number 
  | File String
  | Write
  | Read

data TestState
  = NotebookState Number Number
  | FileState String Boolean

instance tstStTestState :: RouteState TestState where
  toHash (NotebookState foo bar) = "notebook?foo=" <> show foo <>
                                   "&bar=" <> show bar
  toHash (FileState id read) = "file/" <> id <> if read then "read" else "write"

instance tstRouteMsg :: RouteMsg Test where
  toMsg (Tuple "write" _) = Just Write
  toMsg (Tuple "read" _) = Just Read
  toMsg (Tuple "file" map) = do
    f <- lookup "id" map
    pure $ File f 
  toMsg (Tuple "notebook" map) = do
    foo <- readFloat <$> lookup "foo" map
    bar <- readFloat <$> lookup "bar" map
    if isNaN foo || isNaN bar then
      Nothing
      else
      pure $ Notebook foo bar
  toMsg _  = Nothing


main = do
  fprint $ do
    r <- routing
    runRoute "notebook?foo=1&bar=2" r
  let fp :: Tuple Test [Test] -> Eff _ Unit
      fp t = void $ fprint t
  case routing of
    Right r -> do
      routes r $ \r -> void do
        fp r
        setRouteState $ NotebookState 123 234
    _ -> pure unit
                      
