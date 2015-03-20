module Routing.Match where

import Data.Either
import Data.Tuple
import Data.Maybe
import Data.List
import Control.Alt
import Control.Plus
import Control.Apply
import Control.Alternative
import Control.Monad.Error 
import qualified Data.StrMap as M
import Global (readFloat, isNaN)

import Routing.Parser
import Routing.Types
import Routing.Match.Class

newtype Match a = Match (Route -> Either String (Tuple Route a))

instance matchMatchClass :: MatchClass Match where
  lit input = Match $ \route ->
    case route of
      -- TODO: check if (Path input):rs works probably ps bug.
      Cons (Path i) rs | i == input ->
        Right $ Tuple rs unit
      Cons (Path _) _ -> 
        Left <<< strMsg $ "expected path part \"" <> input <> "\""
      _ -> Left <<< strMsg $ "expected path part - found query"
  num = Match $ \route ->
    case route of
      Cons (Path input) rs -> 
        let res = readFloat input in
        if isNaN res then Left <<< strMsg $ "expected numeric var"
        else Right $ Tuple rs res
      _ -> Left <<< strMsg $ "expected numeric var"

  bool = Match $ \route ->
    case route of
      Cons (Path input) rs | input == "true" ->
        Right $ Tuple rs true
      Cons (Path input) rs | input == "false" -> 
        Right $ Tuple rs false
      _ -> Left <<< strMsg $ "expected boolean var"

  str = Match $ \route ->
    case route of
      Cons (Path input) rs -> 
        Right $ Tuple rs input 
      _ -> Left <<< strMsg $ "expected simple var - found query"




  param key = Match $ \route ->
    case route of
      Cons (Query map) rs ->
        case M.lookup key map of
          Nothing -> Left <<< strMsg $ "key " <> key <> " not found in query"
          Just el -> Right $ Tuple (Cons (Query <<< M.delete key $ map) rs) el
      _ -> Left <<< strMsg $ "expected query - found path"
  fail msg = Match \_ -> Left $ strMsg msg

  

instance matchFunctor :: Functor Match where
  (<$>) fn (Match r2e) = Match $ \r -> do
    Tuple rs a <- r2e r
    pure $ Tuple rs (fn a)

instance matchAlt :: Alt Match where
  (<|>) (Match r2e1) (Match r2e2) = Match $ \r -> do
    (r2e1 r) <|> (r2e2 r)

instance matchPlus :: Plus Match where
  empty = Match $ const $ Left noMsg

instance matchAlternative :: Alternative Match

instance matchApply :: Apply Match where
  (<*>) (Match r2a2b) (Match r2a) = Match $ \r -> do
    Tuple rs fn <- r2a2b r
    Tuple rss a <- r2a rs
    pure $ Tuple rss (fn a)

instance matchApplicative :: Applicative Match where
  pure a = Match \r -> Right $ Tuple r a





runMatch :: forall a. Match a -> Route -> Either String a
runMatch (Match fn) route = snd <$> fn route
                   
    
