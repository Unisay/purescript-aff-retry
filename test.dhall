let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "test/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "avar"
              , "console"
              , "foldable-traversable"
              , "test-unit"
              , "tuples"
              ]
        }
