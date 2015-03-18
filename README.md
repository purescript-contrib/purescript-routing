# purescript-routing

[![Build Status](https://travis-ci.org/cryogenian/purescript-routing.svg?branch=master)](https://travis-ci.org/cryogenian/purescript-routing)

Library to handle hash routing for purescript

define route:

```purescript
routing :: PErr Route
routing = do
  notebook <- route "notebook" "notebook?foo&bar"
  file <- route "file" "file/:id"
  read <- route "read" "/read"
  write <- route "write" "/write"
  pure $ notebook `or` (file `contains` (read `or` write))
  ```
  
It will match
* `notebook?foo=12&bar=23` and `notebook?bar=12&foo=123`
* `file/123` 
* `file/123/write` 
* `file/123/read` 

When hash changes from something to `file/123/read` messages of `file` routing
and `read` will be produced. When hash changes from `file/123` to `file/123/read` 
only one message will be produced. 

To get this messages it's necessary to define instance of `RouteDiff` i.e.
```purescript
instance tstRouteDiff :: RouteDiff Test where
  fromMatch (Tuple "write" _) = Just Write
  fromMatch (Tuple "read" _) = Just Read
  fromMatch (Tuple "file" map) = do
    f <- lookup "id" map
    pure $ File f 
  fromMatch (Tuple "notebook" map) = do
    foo <- readFloat <$> lookup "foo" map
    bar <- readFloat <$> lookup "bar" map
    if isNaN foo || isNaN bar then
      Nothing
      else
      pure $ Notebook foo bar
  fromMatch _  = Nothing
```

then you can to use it with something like that 
```purescript
main = do
  let fp :: Tuple Test [Test] -> Eff _ Unit
      fp t = void $ fprint t
  case routing of
    Right r -> do
      routes r $ \r -> void do
        fp r
    _ -> pure unit
```

If you have a state of application you can define instance of `RouteState` then 
you can use `setRouteState` to update hash. 
