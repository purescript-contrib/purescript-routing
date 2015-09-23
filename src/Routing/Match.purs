module Routing.Match where

import Prelude
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), snd)
import Data.Maybe (Maybe(..))
import Data.List (List(..), reverse)
import Control.Alt (Alt, (<|>))
import Control.Plus (Plus)
import Control.Alternative (Alternative)
import Control.Monad.Except
import Global (readFloat, isNaN)
import Data.Semiring.Free (Free(), free, runFree)
import Data.Foldable
import Data.Validation.Semiring


import qualified Data.Map as M
import qualified Data.String as S

import Routing.Parser
import Routing.Types
import Routing.Match.Class
import Routing.Match.Error

newtype Match a = Match (Route -> V (Free MatchError) (Tuple Route a))
unMatch :: forall a. Match a -> (Route -> V (Free MatchError) (Tuple Route a))
unMatch (Match a) = a

instance matchMatchClass :: MatchClass Match where
  lit input = Match $ \route ->
    case route of
      Cons (Path i) rs | i == input ->
        pure $ Tuple rs unit
      Cons (Path _) rs ->
        invalid $ free $  UnexpectedPath input
      _ ->
        invalid $ free ExpectedPathPart
  num = Match $ \route ->
    case route of
      Cons (Path input) rs ->
        let res = readFloat input in
        if isNaN res then
          invalid $ free ExpectedNumber
        else
          pure $ Tuple rs res
      _ ->
        invalid $ free ExpectedNumber

  bool = Match $ \route ->
    case route of
      Cons (Path input) rs | input == "true" ->
        pure $ Tuple rs true
      Cons (Path input) rs | input == "false" ->
        pure $ Tuple rs false
      _ ->
        invalid $ free ExpectedBoolean

  str = Match $ \route ->
    case route of
      Cons (Path input) rs ->
        pure $ Tuple rs input
      _ ->
        invalid $ free ExpectedString

  param key = Match $ \route ->
    case route of
      Cons (Query map) rs ->
        case M.lookup key map of
          Nothing ->
            invalid $ free $ KeyNotFound key
          Just el ->
            pure $ Tuple (Cons (Query <<< M.delete key $ map) rs) el
      _ ->
        invalid $ free ExpectedQuery
  fail msg = Match \_ ->
    invalid $ free $ Fail msg

instance matchFunctor :: Functor Match where
  map fn (Match r2e) = Match $ \r ->
    runV invalid (\(Tuple rs a) -> pure $ Tuple rs (fn a)) $ r2e r

instance matchAlt :: Alt Match where
  alt (Match r2e1) (Match r2e2) = Match $ \r -> do
    (r2e1 r) <|> (r2e2 r)

instance matchPlus :: Plus Match where
  empty = Match $ const $ invalid one

instance matchAlternative :: Alternative Match

instance matchApply :: Apply Match where
  apply (Match r2a2b) (Match r2a) =
    Match $ (\r -> runV (processFnErr r) processFnRes (r2a2b r))
    where processFnErr r err =
            invalid $ err * runV id (const one) (r2a r)
          processFnRes (Tuple rs a2b) =
            runV invalid (\(Tuple rss a) -> pure $ Tuple rss (a2b a)) (r2a rs)

instance matchApplicative :: Applicative Match where
  pure a = Match \r -> pure $ Tuple r a

-- | Matches list of matchers. Useful when argument can easy fail (not `str`)
-- | returns `Match Nil` if no matches
list :: forall a. Match a -> Match (List a)
list (Match r2a) =
  Match $ go Nil
  where go :: List a -> Route -> V (Free MatchError) (Tuple Route (List a))
        go accum r =
          runV
          (const $ pure (Tuple r (reverse accum)))
          (\(Tuple rs a) -> go (Cons a accum) rs)
          (r2a r)




-- It groups `Free MatchError` -> [[MatchError]] -map with showMatchError ->
-- [[String]] -fold with semicolon-> [String] -fold with newline-> String
runMatch :: forall a. Match a -> Route -> Either String a
runMatch (Match fn) route =
  runV foldErrors (Right <<< snd) $ fn route
  where
  foldErrors errs =
    Left $ foldl (\b a -> a <> "\n" <> b) "" do
      es <- reverse <$> runFree errs
      pure $ foldl (\b a -> a <> ";" <> b) "" $ showMatchError <$>  es


-- | if we match something that can fail then we have to
-- | match `Either a b`. This function converts matching on such
-- | sum to matching on right subpart. Matching on left branch fails.
-- | i.e.
-- | ```purescript
-- | data Sort = Asc | Desc
-- | sortOfString :: String -> Either String Sort
-- | sortOfString "asc" = Right Asc
-- | sortOfString "desc" = Right Desc
-- | sortOfString _ = Left "incorrect sort"
-- |
-- | newtype Routing = Routing Sort
-- | routes :: Match Routing
-- | routes = (pure Routing) <*> (eitherMatch (sortOfString <$> var))
-- |
-- | ```
eitherMatch :: forall a b. Match (Either a b) -> Match b
eitherMatch (Match r2eab) = Match $ \r ->
  runV invalid runEither $ (r2eab r)
  where
  runEither (Tuple rs eit) =
    case eit of
      Left _ -> invalid $ free $ Fail "Nested check failed"
      Right res -> pure $ Tuple rs res
