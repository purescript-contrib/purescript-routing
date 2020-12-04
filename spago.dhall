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
  , "numbers"
  , "integers"
  , "lists"
  , "maybe"
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
