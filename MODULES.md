# Module Documentation

## Module Routing

#### `hashChanged`

``` purescript
hashChanged :: forall e. (String -> String -> Eff e Unit) -> Eff e Unit
```


#### `hashes`

``` purescript
hashes :: forall e. (String -> String -> Eff e Unit) -> Eff e Unit
```


#### `matches`

``` purescript
matches :: forall e a. Match a -> (Maybe a -> a -> Eff e Unit) -> Eff e Unit
```

Stream of hash changed, callback called when new hash can be matched
First argument of callback is `Just a` when old hash can be matched
and `Nothing` when it can't.

#### `matches'`

``` purescript
matches' :: forall e a. (String -> String) -> Match a -> (Maybe a -> a -> Eff e Unit) -> Eff e Unit
```


#### `matchesAff'`

``` purescript
matchesAff' :: forall e a. (String -> String) -> Match a -> Aff e (Tuple (Maybe a) a)
```


#### `matchesAff`

``` purescript
matchesAff :: forall e a. Match a -> Aff e (Tuple (Maybe a) a)
```


#### `matchHash`

``` purescript
matchHash :: forall a. Match a -> String -> Either String a
```


#### `matchHash'`

``` purescript
matchHash' :: forall a. (String -> String) -> Match a -> String -> Either String a
```



## Module Routing.Hash

#### `setHash`

``` purescript
setHash :: forall e. String -> Eff (dom :: DOM | e) Unit
```


#### `getHash`

``` purescript
getHash :: forall e. Eff (dom :: DOM | e) String
```


#### `modifyHash`

``` purescript
modifyHash :: forall e. (String -> String) -> Eff (dom :: DOM | e) Unit
```



## Module Routing.Match

#### `Match`

``` purescript
newtype Match a
  = Match (Route -> V (Free MatchError) (Tuple Route a))
```


#### `matchMatchClass`

``` purescript
instance matchMatchClass :: MatchClass Match
```


#### `matchFunctor`

``` purescript
instance matchFunctor :: Functor Match
```


#### `matchAlt`

``` purescript
instance matchAlt :: Alt Match
```


#### `matchPlus`

``` purescript
instance matchPlus :: Plus Match
```


#### `matchAlternative`

``` purescript
instance matchAlternative :: Alternative Match
```


#### `matchApply`

``` purescript
instance matchApply :: Apply Match
```


#### `matchApplicative`

``` purescript
instance matchApplicative :: Applicative Match
```


#### `list`

``` purescript
list :: forall a. Match a -> Match (List a)
```

Matches list of matchers. Useful when argument can easy fail (not `str`)
returns `Match Nil` if no matches

#### `runMatch`

``` purescript
runMatch :: forall a. Match a -> Route -> Either String a
```

#### `eitherMatch`

``` purescript
eitherMatch :: forall a b. Match (Either a b) -> Match b
```

if we match something that can fail then we have to 
match `Either a b`. This function converts matching on such
sum to matching on right subpart. Matching on left branch fails.
i.e.
```purescript
data Sort = Asc | Desc
sortOfString :: String -> Either String Sort
sortOfString "asc" = Right Asc
sortOfString "desc" = Right Desc
sortOfString _ = Left "incorrect sort"
           
newtype Routing = Routing Sort
routes :: Match Routing
routes = (pure Routing) <*> (eitherMatch (sortOfString <$> var))
           
```


## Module Routing.Parser

#### `parse`

``` purescript
parse :: (String -> String) -> String -> Route
```

Parse hash string to `Route` with `decoder` function
applied to every hash part (usually `decodeURIComponent`)


## Module Routing.Types

#### `RoutePart`

``` purescript
data RoutePart
  = Path String
  | Query (M.StrMap String)
```


#### `Route`

``` purescript
type Route = List RoutePart
```



## Module Routing.Hash.Aff

#### `modifyHash`

``` purescript
modifyHash :: forall e. (String -> String) -> Aff (dom :: DOM | e) Unit
```


#### `setHash`

``` purescript
setHash :: forall e. String -> Aff (dom :: DOM | e) Unit
```



## Module Routing.Match.Class

#### `MatchClass`

``` purescript
class (Alternative f) <= MatchClass f where
  lit :: String -> f Unit
  str :: f String
  param :: String -> f String
  num :: f Number
  bool :: f Boolean
  fail :: forall a. String -> f a
```



## Module Routing.Match.Error

#### `MatchError`

``` purescript
data MatchError
  = UnexpectedPath String
  | ExpectedBoolean 
  | ExpectedNumber 
  | ExpectedString 
  | ExpectedQuery 
  | ExpectedPathPart 
  | KeyNotFound String
  | Fail String
```


#### `showMatchError`

``` purescript
showMatchError :: MatchError -> String
```




