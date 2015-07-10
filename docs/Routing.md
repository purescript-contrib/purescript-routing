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


