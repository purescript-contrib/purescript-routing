{ name = "routing"
, dependencies =
  [ "aff"
  , "assert"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "generics-rep"
  , "integers"
  , "js-uri"
  , "lists"
  , "maybe"
  , "numbers"
  , "partial"
  , "prelude"
  , "psci-support"
  , "record"
  , "semirings"
  , "tuples"
  , "validation"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
