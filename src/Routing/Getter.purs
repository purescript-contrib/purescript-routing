-- | Getting `hashchange` events and wraps 
-- | `oldURL` and `newURL` to RouteMatch
-- | or RouteDiff instances 
module Routing.Getter (
  route,
  or,
  contains,
  runRouter,
  hashes,
  matches,
  routes,
  PErr(),
  Router(),
  RouteDiff,
  fromMatch,
  RouteMatch(),
  RouteMatches()
  ) where

import Data.Either
import Data.Maybe
import Data.Tuple
import Control.Monad.Eff
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.State.Class
import Control.Monad.Error
import Data.Monoid
import Control.Apply ((*>))
import Control.Alt ((<|>))
import Data.String.Regex (replace, noFlags, Regex(), regex)
import Data.Array (findIndex, drop, take)
import Data.Traversable (traverse)

import qualified Data.StrMap as M
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P

import Routing.Parser (parse, template, StateObj())

-- | Shortcut for parser
type RouteParser = P.ParserT String StateObj RouteMatches
-- | Main data produced by matching hash
-- | Tuple routeName mappedTemplateArgumentsValues
type RouteMatch = Tuple String (M.StrMap String)
-- | nonempty list of matches
type RouteMatches = Tuple RouteMatch [RouteMatch]
-- | Shortcut for parsing errors
type PErr a = Either P.ParseError a
-- | 
newtype Router = Router RouteParser

-- | check if `s` matches router
runRouter :: String -> Router -> PErr RouteMatches
runRouter s (Router p) =
  evalState (P.runParserT s p) M.empty 

-- | define router by name and template
route :: String -> String -> PErr Router
route name templateStr = do
  parser <- parse <$> P.runParser templateStr template
  let p = parser *> lift get >>= \res ->
        pure $ Tuple (Tuple name res) []
  pure $ Router p
  
-- | construct router from two routers
-- | will match first **or** second
or :: Router -> Router -> Router
or (Router one) (Router two) =
  Router $ one <|> two

-- | construct router from two routers
-- | will try to match first router and
-- | if it matches try to match second
-- | produced result of matching first if
-- | second fails or aggregated result otherwise
contains :: Router -> Router -> Router
contains (Router parent) (Router child) =
  let put' :: M.StrMap String -> StateObj Unit
      put' = put 
  in Router $ do
    Tuple pHead pTail <- parent
    lift $ put' M.empty
    c <- P.optionMaybe child
    pure $ case c of
      Nothing -> Tuple pHead pTail
      Just (Tuple cHead []) -> Tuple cHead (pHead:pTail)

-- | Main driver of routing 
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

-- | stream of hashes without hash symbol
hashes :: forall e. (String -> String -> Eff e Unit) -> Eff e Unit
hashes cb =
  hashChanged $ \old new -> do
    cb (dropHash old) (dropHash new)
  where dropHash h = replace (regex "^[^#]+#" noFlags) "" h

-- | Class of types that can be produced by matching
-- | diffs of hashes
class RouteDiff a where 
  fromMatch :: RouteMatch -> Maybe a

-- | Tuple name argValMap is just RouteMatch
instance strMap :: RouteDiff (Tuple String (M.StrMap String)) where
  fromMatch = Just

-- | Stream of `RouteMatch`es 
matches :: forall e. Router -> (RouteMatches -> Eff e Unit) -> Eff e Unit
matches route callback =  
  hashes $ \old new -> do
    let diff = do
          -- Get old url matches 
          Tuple headOld olds <- runRouter old route
          -- Get new url matches
          Tuple headNew news <- runRouter new route
          -- if new url is substate of old url
          case findIndex ((==) headOld) news of
            -1 -> pure $ Tuple headNew news
            -- find depth of substate and return only new part 
            i -> if drop i news == (headOld:olds) then
                   pure $ Tuple headNew (take i news)
                   else
                   pure $ Tuple headNew news 
    case diff of
      -- If router matches hash then callback
      Right matches -> callback matches
      -- Otherwise do nothing
      _ -> pure unit

-- Stream of decoded messages. i.e. in case of
-- purescript-halogen `i` from `HTML a i`
routes :: forall e a. (RouteDiff a) =>
          Router -> (Tuple a [a] -> Eff e Unit) -> Eff e Unit
routes route callback =
  matches route $ \(Tuple match matches) -> 
    maybe (pure unit) callback do
          r <- fromMatch match
          rs <- traverse fromMatch matches
          pure $ Tuple r rs

-- | Router is semigroup over `or`
instance semigroupRouter :: Semigroup Router where
  (<>) = or
-- | Router is monoid with always failing router as `mempty`
instance monoidRouter :: Monoid Router where
  mempty = Router $ P.fail "mempty"
