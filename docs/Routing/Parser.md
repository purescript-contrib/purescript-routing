## Module Routing.Parser

#### `parse`

``` purescript
parse :: (String -> String) -> String -> Route
```

Parse hash string to `Route` with `decoder` function
applied to every hash part (usually `decodeURIComponent`)


