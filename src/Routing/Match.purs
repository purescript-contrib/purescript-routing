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
import Data.Semiring.Free
import Data.Foldable
import qualified Data.Array as A 
import Data.Validation.Alt

import Routing.Parser
import Routing.Types
import Routing.Match.Class
import Routing.Match.Error

newtype Match a = Match (Route -> V (Free MatchError) (Tuple Route a)) 

instance matchMatchClass :: MatchClass Match where
  lit input = Match $ \route ->
    case route of
      Cons (Path i) rs | i == input ->
        Valid $ Tuple rs unit
      Cons (Path _) rs ->
        Invalid $ free $  UnexpectedPath input
      _ ->
        Invalid $ free ExpectedPathPart
  num = Match $ \route ->
    case route of
      Cons (Path input) rs -> 
        let res = readFloat input in
        if isNaN res then
          Invalid $ free ExpectedNumber
        else
          Valid $ Tuple rs res 
      _ ->
        Invalid $ free ExpectedNumber

  bool = Match $ \route ->
    case route of
      Cons (Path input) rs | input == "true" ->
        Valid $ Tuple rs true 
      Cons (Path input) rs | input == "false" ->
        Valid $ Tuple rs false 
      _ ->
        Invalid $ free ExpectedBoolean

  str = Match $ \route ->
    case route of
      Cons (Path input) rs ->
        Valid $ Tuple rs input
      _ ->
        Invalid $ free ExpectedString

  param key = Match $ \route ->
    case route of
      Cons (Query map) rs ->
        case M.lookup key map of
          Nothing ->
            Invalid $ free $ KeyNotFound key
          Just el ->
            Valid $ Tuple (Cons (Query <<< M.delete key $ map) rs) el
      _ ->
        Invalid $ free ExpectedQuery
  fail msg = Match \_ ->
    Invalid $ free $ Fail msg

instance matchFunctor :: Functor Match where
  (<$>) fn (Match r2e) = Match $ \r ->
    case r2e r of
      Invalid a -> Invalid a
      Valid (Tuple rs a) -> Valid (Tuple rs (fn a))


instance matchAlt :: Alt Match where
  (<|>) (Match r2e1) (Match r2e2) = Match $ \r -> do
    (r2e1 r) <|> (r2e2 r)
instance matchPlus :: Plus Match where
  empty = Match $ const $ Invalid one

instance matchAlternative :: Alternative Match

instance matchApply :: Apply Match where
  (<*>) (Match r2a2b) (Match r2a) = Match $ \r -> do
    case r2a2b r of
      Invalid err ->
        case r2a r of
          Invalid err' -> Invalid (err * err')
          _ -> Invalid err
      Valid (Tuple rs a2b)  ->
        case r2a rs of -- `rs` here not r, so we can't use `<*>` from `V`
          Invalid err -> Invalid err
          Valid (Tuple rss a) -> Valid $ Tuple rss (a2b a)

instance matchApplicative :: Applicative Match where
  pure a = Match \r -> pure $ Tuple r a

-- It groups `Free MatchError` -> [[MatchError]] -map with showMatchError ->
-- [[String]] -fold with semicolon-> [String] -fold with newline-> String 
runMatch :: forall a. Match a -> Route -> Either String a
runMatch (Match fn) route =
  case fn route of
    Valid res -> Right $ snd res
    Invalid errs -> Left $ foldl (\b a -> a <> "\n" <> b) "" do
      es <- A.reverse <$> runFree errs
      pure $ foldl (\b a -> a <> ";" <> b) "" $ showMatchError <$>  es
    
