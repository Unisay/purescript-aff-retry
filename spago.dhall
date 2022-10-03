{ name = "aff-retry"
, license = "MIT"
, repository = "https://github.com/Unisay/purescript-aff-retry"
, dependencies =
  [ "aff"
  , "arrays"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "integers"
  , "maybe"
  , "newtype"
  , "numbers"
  , "prelude"
  , "random"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
