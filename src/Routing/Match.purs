module Routing.Match where

import Data.Either
import Data.Tuple
import Data.Maybe
import Control.Alt
import Control.Plus
import Control.Apply
import Control.MonadPlus
import Control.Alternative
import Control.Monad.Error 
import qualified Data.StrMap as M

import Routing.Parser
import Routing.Types
import Routing.Match.Class

newtype Match a = Match (Route -> Either String (Tuple Route a))

instance matchMatchClass :: MatchClass Match where
  lit input = Match $ \route ->
    case route of
      -- TODO: check if (Path input):rs works probably ps bug.
      (Path i):rs |i == input ->
        Right $ Tuple rs unit
      (Path _):_ -> Left <<< strMsg $ "expected path part \"" <> input <> "\""
      _ -> Left <<< strMsg $ "expected path part - found query"
  var = Match $ \route ->
    case route of
      (Path input):rs -> Right $ Tuple rs input 
      _ -> Left <<< strMsg $ "expected simple var - found query"

  param key = Match $ \route ->
    case route of
      (Query map):rs ->
        case M.lookup key map of
          Nothing -> Left <<< strMsg $ "key " <> key <> " not found in query"
          Just el -> Right $ Tuple ((Query <<< M.delete key $ map):rs) el
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

instance matchBind :: Bind Match where
  (>>=) (Match r2a) a2mb = Match $ \r -> do
    Tuple rs a <- r2a r
    case a2mb a of
      Match res -> res rs

instance matchMonad :: Monad Match
instance matchMonadPlus :: MonadPlus Match


runMatch :: forall a. Match a -> Route -> Either String a
runMatch (Match fn) route = snd <$> fn route
                   
    
