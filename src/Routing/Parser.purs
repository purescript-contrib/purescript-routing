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
data TemplateEl = Placeholder String | Key String | Ask [String]
-- | shortcut
type Template = [TemplateEl]
-- | shortcut 
type StateObj = State (M.StrMap String)

-- | Parses placeholder names
name :: forall m. (Monad m) => P.ParserT String m String
name = do
  strs <- many $ P.noneOf [":", "&", "?"]
  case strs of
    [] -> P.fail "empty name"
    cs -> return (fold cs)
    
-- | parses variables 
variable :: forall m. (Monad m) => P.ParserT String m String
variable = do
  strs <- many $ P.noneOf [":", "&", "/", "?", "="]
  case strs of
    [] -> P.fail "empty var"
    cs -> return (fold cs)
    
-- | parses placeholder
placeholder :: forall m. (Monad m) => P.ParserT String m TemplateEl
placeholder =  Placeholder <$> name 

-- | parses variables in `:foo`
colon :: forall m. (Monad m) => P.ParserT String m TemplateEl
colon = do
  P.string ":"
  Key <$> variable

-- | parses variables in `?foo&bar&baz`
query :: forall m. (Monad m) => P.ParserT String m TemplateEl
query = do
  strs <- many $ do
    P.string "&" <|> P.string "?"
    variable
  case strs of
    [] -> P.fail "not an query string"
    qs -> pure $ Ask qs

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
    [] -> pure unit
    
    (Key str):ts -> do
      g <- lift $ get' str
      v <- variable
      case g of
        Nothing -> do
          lift $ modify (M.insert str v)
          parse ts
        _ -> P.fail $ "duplicated key '" <> str <> "' is unmatching"
        
    (Ask strs):ts -> do
      P.string "?"
      res <- many $ do
        (P.try $ P.string "&") <|> pure ""
        s <- P.choice $ P.try <<< P.string <$> strs
        P.string "="
        v <- variable
        g <- lift $ get' s
        lift $ modify (M.insert s v)
        case g of
          Nothing -> 
            pure v
          _ -> 
            P.fail "query contains duplicated keys"
      parse ts
      
    (Placeholder str):ts -> do
      P.string str
      parse ts
