{-
{{GENERATED_DOC}}

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
      , "const"
      , "formlet"
      , "halogen"
      , "halogen-subscriptions"
      , "halogen-test-driver"
      , "pre"
      , "prelude"
      , "test-unit"
      , "variant"
      ]
  -- This path is relative to config file
  , packages = {{PACKAGES_DIR}}/packages.dhall
  -- This path is relative to project root
  -- See https://github.com/purescript/spago/issues/648
  , sources = [ "{{SOURCES_DIR}}/src/**/*.purs", "{{SOURCES_DIR}}/test/**/*.purs" ]
  }
