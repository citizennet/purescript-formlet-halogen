{-
The `formlet-halogen` package contains Halogen-specific implementations for the `formlet`
abstractions. This includes components for rendering forms and functions for
transforming render functors into Halogen HTML.
-}
let
  name = "formlet-halogen"
in
  { name
  , dependencies =
      [ "aff"
      , "arrays"
      , "bifunctors"
      , "cn-debug"
      , "const"
      , "formlet"
      , "halogen"
      , "halogen-subscriptions"
      , "halogen-test-driver"
      , "pre"
      , "prelude"
      , "test-unit"
      , "test-utils"
      , "variant"
      ]
  , packages = ../../packages.dhall
  -- Due to a spago bug (see https://github.com/purescript/spago/issues/648)
  -- `sources` are relative to root instead of config file.
  , sources = [ "lib/${name}/src/**/*.purs", "lib/${name}/test/**/*.purs" ]
  }
