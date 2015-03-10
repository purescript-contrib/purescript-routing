module Routing.Getter (
  route,
  or,
  contains,
  runRoute,
  hashes,
  checks,
  routes,
  PErr(),
  Route(),
  RouteMsg,
  toMsg,
  Check(),
  Checks()
  ) where

import Data.Either
import Data.Maybe
import Data.Tuple
import Control.Monad.Eff
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.State.Class
import Control.Monad.Error
import Control.Apply ((*>))
import Control.Alt ((<|>))
import Data.String.Regex (replace, noFlags, Regex(), regex)
import Data.Array (findIndex, drop, take)
import Data.Traversable (traverse)

import qualified Data.StrMap as M
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P

import Routing.Parser (parse, template, StateObj())

type RouteParser = P.ParserT String StateObj Checks
type Check = Tuple String (M.StrMap String)
type Checks = Tuple Check [Check]
type PErr a = Either P.ParseError a 
newtype Route = Route RouteParser

runRoute :: String -> Route -> PErr Checks
runRoute s (Route p) =
  case runState (P.runParserT s p) M.empty of
    Tuple lr state -> lr

             
route :: String -> String -> PErr Route
route name templateStr = do
  parser <- parse <$> P.runParser templateStr template
  let p = parser *> lift get >>= \res ->
        pure $ Tuple (Tuple name res) []
  pure $ Route p

or :: Route -> Route -> Route
or (Route one) (Route two) =
  Route $ one <|> two

contains :: Route -> Route -> Route
contains (Route parent) (Route child) =
  let put' :: M.StrMap String -> StateObj Unit
      put' = put 
  in Route $ do
    Tuple pHead parentChecks <- parent
    lift $ put' M.empty
    c <- P.optionMaybe child
    pure $ case c of
      Nothing -> Tuple pHead parentChecks
      Just (Tuple cHead []) -> Tuple cHead (pHead:parentChecks)


foreign import hashChanged """
function hashChanged(handler) {
  return function() {
    var currentHash = document.location.hash;
    handler("")(currentHash)();
    window.addEventListener("hashchange", function(ev) {
      handler(ev.oldURL)(ev.newURL)();
    });
  };
}
""" :: forall e. (String -> String -> Eff e Unit) -> Eff e Unit

rgx :: Regex
rgx = regex "^[^#]+#" noFlags 

hashes :: forall e. (String -> String -> Eff e Unit) -> Eff e Unit
hashes cb =
  hashChanged $ \old new -> do
    cb (replace rgx "" old) (replace rgx "" new)

class RouteMsg a where 
  toMsg :: Check -> Maybe a

instance strMap :: RouteMsg (Tuple String (M.StrMap String)) where
  toMsg = Just

checks :: forall e. Route -> (Checks -> Eff e Unit) -> Eff e Unit
checks route callback =  
  hashes $ \old new -> do
    let diff = do
          Tuple headOld olds <- runRoute old route
          Tuple headNew news <- runRoute new route
          case findIndex ((==) headOld) news of
            -1 -> pure $ Tuple headNew news
            i -> if drop i news == (headOld:olds) then
                   pure $ Tuple headNew (take i news)
                   else
                   pure $ Tuple headNew news 
    case diff of
      Right checks -> callback checks
      _ -> pure unit

routes :: forall e a. (RouteMsg a) =>
          Route -> (Tuple a [a] -> Eff e Unit) -> Eff e Unit
routes route callback =
  checks route $ \(Tuple check checks) -> 
    maybe (pure unit) callback do
          r <- toMsg check
          rs <- traverse toMsg checks
          pure $ Tuple r rs

