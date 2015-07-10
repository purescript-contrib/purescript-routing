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


