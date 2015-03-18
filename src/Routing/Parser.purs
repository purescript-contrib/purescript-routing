module Routing.Parser (
  parse,
  template,
  StateObj(),
  TemplateEl(..),
  Template()
  ) where

import Data.Maybe
import Data.Either

import Control.Alternative (many)
import Control.Alt ((<|>))
import Data.Foldable (fold, foldl, sequence_, traverse_)
import Data.Traversable (sequence, traverse)
import Control.Monad.State 
import Control.Monad.Trans
import Control.Monad.State.Class

import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.String as P
import qualified Data.StrMap as M


-- | Ast of parsed route template
data TemplateEl = Placeholder String | Variable String | Query [String]
-- | shortcut
type Template = [TemplateEl]
-- | shortcut 
type StateObj = State (M.StrMap String)


foreign import decodeURIComponent :: String -> String

-- | Parses placeholder names
name :: forall m. (Monad m) => P.ParserT String m String
name = do
  strs <- many $ P.noneOf [":", "&", "?"]
  case strs of
    [] -> P.fail "empty name"
    cs -> return $ decodeURIComponent (fold cs)
    
-- | parses variables 
variable :: forall m. (Monad m) => P.ParserT String m String
variable = do
  strs <- many $ P.noneOf [":", "&", "/", "?", "="]
  case strs of
    [] -> P.fail "empty var"
    cs -> return $ decodeURIComponent (fold cs)
    
-- | parses placeholder
placeholder :: forall m. (Monad m) => P.ParserT String m TemplateEl
placeholder =  Placeholder <$> name 

-- | parses variables in `:foo`
colon :: forall m. (Monad m) => P.ParserT String m TemplateEl
colon = do
  P.string ":"
  Variable <$> variable

-- | parses variables in `?foo&bar&baz`
query :: forall m. (Monad m) => P.ParserT String m TemplateEl
query = do
  strs <- many $ do
    P.string "&" <|> P.string "?"
    variable
  case strs of
    [] -> P.fail "not an query string"
    qs -> pure $ Query qs

-- | parses all template elements
template :: forall m. (Monad m) => P.ParserT String m Template
template = many $ P.choice [
  P.try placeholder,
  P.try query,
  P.try colon]


-- | Produce parsers of uri from template strings
parse :: Template -> P.ParserT String StateObj Unit
parse template =
  -- type hints
  let get' :: String -> StateObj (Maybe String)
      get' str = gets (M.lookup str)
      get'' :: StateObj (M.StrMap String)
      get'' = get
      
    in case template of
    -- ast is over 
    [] -> pure unit
    (Variable str):ts -> do
      -- lookup this variable setted in state
      -- fail if it has been setted
      g <- lift $ get' str
      v <- variable
      case g of
        Nothing -> do
          -- insert key val pair to state
          lift $ modify (M.insert str v)
          parse ts
        _ -> P.fail $ "Parsing route template error: duplicated key " <>
             show str 
        
    (Query strs):ts -> do
      P.string "?"
      res <- many $ do
        -- consume "&" or nothing
        (P.try $ P.string "&") <|> pure ""
        -- find one of template args 
        s <- P.choice $ P.try <<< P.string <$> strs
        P.string "="
        -- check if this variable was setted before
        v <- variable
        g <- lift $ get' s
        case g of
          Nothing -> do
            lift $ modify (M.insert s v)
            pure v
          _ -> 
            P.fail $ "Parsing route template error: duplicated key" <>
            show v <> " in query: " <> show strs 
            
      parse ts
      
    (Placeholder str):ts -> do
      -- consume placeholder
      P.string str
      parse ts
