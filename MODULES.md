# Module Documentation

## Module Routing.Getter



#### `Check`

``` purescript
type Check = Tuple String (M.StrMap String)
```


#### `Checks`

``` purescript
type Checks = Tuple Check [Check]
```


#### `PErr`

``` purescript
type PErr a = Either P.ParseError a
```


#### `Route`

``` purescript
newtype Route
```


#### `runRoute`

``` purescript
runRoute :: String -> Route -> PErr Checks
```


#### `route`

``` purescript
route :: String -> String -> PErr Route
```


#### `or`

``` purescript
or :: Route -> Route -> Route
```


#### `contains`

``` purescript
contains :: Route -> Route -> Route
```


#### `hashes`

``` purescript
hashes :: forall e. (String -> String -> Eff e Unit) -> Eff e Unit
```


#### `RouteMsg`

``` purescript
class RouteMsg a where
  toMsg :: Check -> Maybe a
```


#### `strMap`

``` purescript
instance strMap :: RouteMsg (Tuple String (M.StrMap String))
```


#### `checks`

``` purescript
checks :: forall e. Route -> (Checks -> Eff e Unit) -> Eff e Unit
```


#### `routes`

``` purescript
routes :: forall e a. (RouteMsg a) => Route -> (Tuple a [a] -> Eff e Unit) -> Eff e Unit
```



## Module Routing.Parser



#### `TemplateEl`

``` purescript
data TemplateEl
  = Placeholder String
  | Key String
  | Ask [String]
```

Ast of parsed route template

#### `Template`

``` purescript
type Template = [TemplateEl]
```

shortcut

#### `StateObj`

``` purescript
type StateObj = State (M.StrMap String)
```

shortcut 

#### `template`

``` purescript
template :: forall m. (Monad m) => P.ParserT String m Template
```

parses all template elements

#### `parse`

``` purescript
parse :: Template -> P.ParserT String StateObj Unit
```

Produce parsers of uri from template strings


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


#### `setRouteState`

``` purescript
setRouteState :: forall r e. (RouteState r) => r -> Eff e Unit
```




