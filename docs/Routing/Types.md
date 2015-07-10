## Module Routing.Types

#### `RoutePart`

``` purescript
data RoutePart
  = Path String
  | Query (StrMap String)
```

#### `Route`

``` purescript
type Route = List RoutePart
```


