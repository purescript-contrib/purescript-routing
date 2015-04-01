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


#### `matchHash`

``` purescript
matchHash :: forall a. Match a -> String -> Either String a
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


#### `runMatch`

``` purescript
runMatch :: forall a. Match a -> Route -> Either String a
```

#### `eitherMatch`

``` purescript
eitherMatch :: forall a b. Match (Either a b) -> Match b
```



## Module Routing.Parser

#### `parse`

``` purescript
parse :: String -> Route
```



## Module Routing.Setter

#### `setHash`

``` purescript
setHash :: forall e. String -> Eff e Unit
```


#### `RouteState`

``` purescript
class RouteState a where
  toHash :: a -> String
```

Class of types that can be converted to hashes 

#### `setRouteState`

``` purescript
setRouteState :: forall r e. (RouteState r) => r -> Eff e Unit
```

wrapper over `setHash` that uses `RouteState`


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




