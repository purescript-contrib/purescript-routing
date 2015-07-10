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


