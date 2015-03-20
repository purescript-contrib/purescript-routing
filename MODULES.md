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


#### `log`

``` purescript
log :: forall a e. a -> Eff e Unit
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
  = Match (Route -> Either String (Tuple Route a))
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


#### `matchBind`

``` purescript
instance matchBind :: Bind Match
```


#### `matchMonad`

``` purescript
instance matchMonad :: Monad Match
```


#### `matchMonadPlus`

``` purescript
instance matchMonadPlus :: MonadPlus Match
```


#### `runMatch`

``` purescript
runMatch :: forall a. Match a -> Route -> Either String a
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
type Route = [RoutePart]
```



## Module Routing.Match.Class

#### `MatchClass`

``` purescript
class (MonadPlus f) <= MatchClass f where
  lit :: String -> f Unit
  var :: f String
  param :: String -> f String
  fail :: forall a. String -> f a
```



## Module Routing.Match.Combinators

#### `num`

``` purescript
num :: forall f. (MatchClass f) => String -> f Number
```


#### `bool`

``` purescript
bool :: forall f. (MatchClass f) => String -> f Boolean
```




