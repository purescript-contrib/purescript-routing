module Routing.Match where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Plus (class Plus)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.List (List(..), reverse)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Semiring.Free (Free, free)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.Tuple (Tuple(..), snd)
import Data.Validation.Semiring (V, invalid, validation)
import Data.Number as Number
import Routing.Match.Error (MatchError(..), showMatchError)
import Routing.Types (Route, RoutePart(..))

newtype Match a = Match (Route -> V (Free MatchError) (Tuple Route a))

derive instance newtypeMatch :: Newtype (Match a) _

instance matchFunctor :: Functor Match where
  map fn (Match r2e) = Match $ \r ->
    validation invalid (\(Tuple rs a) -> pure $ Tuple rs (fn a)) $ r2e r

instance matchAlt :: Alt Match where
  alt (Match r2e1) (Match r2e2) = Match $ \r -> do
    (r2e1 r) <|> (r2e2 r)

instance matchPlus :: Plus Match where
  empty = Match $ const $ invalid one

instance matchAlternative :: Alternative Match

instance matchApply :: Apply Match where
  apply (Match r2a2b) (Match r2a) =
    Match $ (\r -> validation (processFnErr r) processFnRes (r2a2b r))
    where
    processFnErr r err =
      invalid $ err * validation identity (const one) (r2a r)

    processFnRes (Tuple rs a2b) =
      validation invalid (\(Tuple rss a) -> pure $ Tuple rss (a2b a)) (r2a rs)

instance matchApplicative :: Applicative Match where
  pure a = Match \r -> pure $ Tuple r a

-- | Matches a leading slash.
root :: Match Unit
root = lit ""

-- | `lit x` will match exactly the path component `x`.
-- | For example, `lit "x"` matches `/x`.
lit :: String -> Match Unit
lit input = Match \route ->
  case route of
    Cons (Path i) rs | i == input ->
      pure $ Tuple rs unit
    Cons (Path _) _ ->
      invalid $ free $ UnexpectedPath input
    _ ->
      invalid $ free ExpectedPathPart

-- | `num` matches any numerical path component.
num :: Match Number
num = Match \route ->
  case route of
    Cons (Path input) rs ->
      case Number.fromString input of
        Just res | not (Number.isNaN res) -> pure $ Tuple rs res
        _ -> invalid $ free ExpectedNumber
    _ ->
      invalid $ free ExpectedNumber

-- | `int` matches any integer path component.
int :: Match Int
int = Match \route ->
  case route of
    Cons (Path input) rs -> case fromString input of
      Nothing -> invalid $ free ExpectedInt
      Just res -> pure $ Tuple rs res
    _ ->
      invalid $ free ExpectedInt

-- | `bool` matches any boolean path component.
bool :: Match Boolean
bool = Match \route ->
  case route of
    Cons (Path input) rs | input == "true" ->
      pure $ Tuple rs true
    Cons (Path input) rs | input == "false" ->
      pure $ Tuple rs false
    _ ->
      invalid $ free ExpectedBoolean

-- | `str` matches any path string component.
-- | For example, `str` matches `/foo` as `"foo"`.
str :: Match String
str = Match \route ->
  case route of
    Cons (Path input) rs ->
      pure $ Tuple rs input
    _ ->
      invalid $ free ExpectedString

-- | `param p` matches a parameter assignment `q=v` within a query block.
-- | For example, `param "q"` matches `/?q=a&r=b` as `"a"`.
param :: String -> Match String
param key = Match \route ->
  case route of
    Cons (Query map) rs ->
      case M.lookup key map of
        Nothing ->
          invalid $ free $ KeyNotFound key
        Just el -> do
          let remainingParams = M.delete key map
          pure $
            if M.isEmpty remainingParams then Tuple rs el
            else Tuple (Cons (Query remainingParams) rs) el
    _ ->
      invalid $ free ExpectedQuery

-- | `params` matches an entire query block. For exmaple, `params`
-- | matches `/?q=a&r=b` as the map `{q : "a", r : "b"}`. Note that
-- | `lit "foo" *> params` does *not* match `/foo`, since a query component
-- | is *required*.
params :: Match (M.Map String String)
params = Match \route ->
  case route of
    Cons (Query map) rs ->
      pure $ Tuple rs map
    _ ->
      invalid $ free ExpectedQuery

-- | `end` matches the end of a route.
end :: Match Unit
end = Match \route ->
  case route of
    Nil -> pure $ Tuple Nil unit
    _ -> invalid $ free ExpectedEnd

fail :: forall a. String -> Match a
fail msg = Match \_ ->
  invalid $ free $ Fail msg

-- | Matches a non-empty string.
nonempty :: Match NonEmptyString
nonempty =
  eitherMatch $ maybe (Left "Empty string") Right <<< NES.fromString <$> str

-- | Matches list of matchers. Useful when argument can easy fail (not `str`)
-- | returns `Match Nil` if no matches
list :: forall a. Match a -> Match (List a)
list (Match r2a) =
  Match $ go Nil
  where
  go :: List a -> Route -> V (Free MatchError) (Tuple Route (List a))
  go accum r =
    validation
      (const $ pure (Tuple r (reverse accum)))
      (\(Tuple rs a) -> go (Cons a accum) rs)
      (r2a r)

-- It groups `Free MatchError` -> [[MatchError]] -map with showMatchError ->
-- [[String]] -fold with semicolon-> [String] -fold with newline-> String
runMatch :: forall a. Match a -> Route -> Either String a
runMatch (Match fn) route =
  validation foldErrors (Right <<< snd) $ fn route
  where
  foldErrors errs =
    Left $ foldl (\b a -> a <> "\n" <> b) "" do
      es <- reverse <$> unwrap errs
      pure $ foldl (\b a -> a <> ";" <> b) "" $ showMatchError <$> es

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
  validation invalid runEither $ (r2eab r)
  where
  runEither (Tuple rs eit) =
    case eit of
      Left _ -> invalid $ free $ Fail "Nested check failed"
      Right res -> pure $ Tuple rs res

-- | useful for matching optional params at the end of a path
-- | ```
-- | optParams = maybe M.empty id <$> optionalMatch params <* end
-- | runMatch (lit "path" *> optParams) (parse id "path/?a=1")
-- | -- (Right (fromFoldable [(Tuple "a" "1")]))
-- | ```
optionalMatch :: forall a. Match a -> Match (Maybe a)
optionalMatch (Match fn) = Match (\route -> validation (const $ pure (Tuple route Nothing)) (pure <<< map Just) $ fn route)
