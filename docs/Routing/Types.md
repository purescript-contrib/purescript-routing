## Module Routing.Types

#### `RoutePart`

``` purescript
data RoutePart
  = Path String
  | Query (Map String String)
```

#### `Route`

``` purescript
type Route = List RoutePart
```


