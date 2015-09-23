## Module Routing.Match

#### `Match`

``` purescript
newtype Match a
  = Match (Route -> V (Free MatchError) (Tuple Route a))
```

##### Instances
``` purescript
instance matchMatchClass :: MatchClass Match
instance matchFunctor :: Functor Match
instance matchAlt :: Alt Match
instance matchPlus :: Plus Match
instance matchAlternative :: Alternative Match
instance matchApply :: Apply Match
instance matchApplicative :: Applicative Match
```

#### `unMatch`

``` purescript
unMatch :: forall a. Match a -> Route -> V (Free MatchError) (Tuple Route a)
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


