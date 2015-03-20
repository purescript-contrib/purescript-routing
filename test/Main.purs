module Main where

import Control.Monad.Eff
import Debug.Trace 

import Control.Alt
import Control.Alternative
import Control.Apply
import Control.Plus
import Control.MonadPlus
import Data.Traversable

import Data.Tuple
import Debug.Foreign
import Data.Maybe
import Data.Either
import qualified Data.Array as A
import qualified Data.StrMap as M
import qualified Data.String as S 
import Global

class (MonadPlus f) <= Match f where
  lit :: String -> f Unit
  var :: f String
  param :: String -> f String
  fail :: forall a. String -> f a


newtype I a = I (Route -> Either String (Tuple Route a))

instance iMatch :: Match I where
  lit input = I $ \route ->
    case route of
      (Path i):rs |i == input ->
        Right $ Tuple rs unit
      q@(Path _):_ -> let t = fprintUnsafe input in Left "not that"
      _ -> Left "not a literal"
  var = I $ \route ->
    case route of
      (Path input):rs -> Right $ Tuple rs input 
      _ -> Left "not a var"
  param key = I $ \route ->
    case route of
      (Query map):rs ->
        case M.lookup key map of
          Nothing -> Left "no param in query"
          Just el -> Right $ Tuple ((Query <<< M.delete key $ map):rs) el
      _ -> Left "not a query"
  fail msg = I \_ -> Left msg


instance iFunctor :: Functor I where
  (<$>) fn (I r2e) = I $ \r -> do
    Tuple rs a <- r2e r
    return $ Tuple rs (fn a)

instance iAlt :: Alt I where
  (<|>) (I r2e1) (I r2e2) = I $ \r -> do
    (r2e1 r) <|> (r2e2 r)

instance iPlus :: Plus I where
  empty = I $ \_ -> Left "empty"

instance iAlternative :: Alternative I

instance iApply :: Apply I where
  (<*>) (I r2a2b) (I r2a) = I $ \route -> do
    Tuple rs fn <- r2a2b route
    Tuple rss a <- r2a rs
    return $ Tuple rss (fn a)

instance iApplicative :: Applicative I where
  pure a = I $ \r -> Right $ Tuple r a

instance iBind :: Bind I where
  (>>=) (I r2a) a2ib = I $ \r -> do
    Tuple rs a <- r2a r 
    case a2ib a of
      I res -> res rs

instance iMonad :: Monad I
instance iMonadPlus :: MonadPlus I

-- Для начала нам похер каким образом построены пары имя-переменная
-- поэтому мы можем тупо положить на то, что и как там у нас в каком-либо
-- порядке организовано
data RoutePart = Path String | Query (M.StrMap String)

type Route = [RoutePart]

tryQueryify :: RoutePart -> RoutePart
tryQueryify source@(Path string) = fromMaybe source $ do
  guard $ S.take 1 string == "?"
  let parts = S.split "&" $ S.drop 1 string
  Query <$> M.fromList <$> traverse onePartToKeyVal parts
  where onePartToKeyVal :: String -> Maybe (Tuple String String) 
        onePartToKeyVal input = do 
          let keyVal = S.split "=" input
          if A.length keyVal > 2 then Nothing
            else 
            Tuple <$> (A.head keyVal) <*> (keyVal A.!! 1)
        
tryQueryify q = q 

parse :: String -> Route
parse hash =
  tryQueryify <$> Path <$> S.split "/" hash




{-
num :: String -> I Number
num input = 
  let res = readFloat input in
  I $ \route -> case isNaN res of
    true -> Left "not a number"
    false -> Right $ Tuple route res
-}
num :: forall f. (Match f) => String -> f Number
num input =
  let res = readFloat input in
  case isNaN res of
    true -> fail "not a number" 
    false -> return res

bool :: forall f. (Match f) => String -> f Boolean
bool input =
  case input of
    "true" -> pure true
    "false" -> pure false
    _ -> fail "not a boolean"

{-
bool :: String -> I Boolean
bool input = I $ \route ->
  case input of
    "true" -> Right $ Tuple route true
    "false" -> Right $ Tuple route false
    _ -> Left "not a boolean"
  -}


--match :: forall a. (forall f. (Match f) => f a) -> Route -> Either String a

runMatch :: forall a. I a -> Route -> Either String a
runMatch (I fn) route = snd <$> fn route

matchHash :: forall a. I a -> String -> Either String a
matchHash matcher hash = runMatch matcher $ parse hash

data FooBar = Foo Number | Bar Boolean String

routing :: I FooBar
--routing :: forall f. (Match f) => f FooBar
routing =
  Foo <$> (lit "foo" *> (var >>= num))
  <|>
  Bar <$> (lit "bar" *> (var >>= bool)) <*> (param "baz")

main = do
  fprint $ parse "bar/asdf/?baz=12"
--  fprint $ matchHash routing "bar/100/?foo=14&baz=12/foo/123"
  fprint $ matchHash routing "bar/asdf/?baz=123"
  fprint $ matchHash routing "foo/asdf/"
