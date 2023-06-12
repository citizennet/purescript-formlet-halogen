module Test.Formlet.Halogen
  ( main
  ) where

import CitizenNet.Prelude

import Data.Const as Data.Const
import Formlet as Formlet
import Formlet.Halogen as Formlet.Halogen
import Formlet.Render as Formlet.Render
import Formlet.Validation as Formlet.Validation
import Halogen.HTML as Halogen.HTML
import Halogen.Test.Driver as Halogen.Test.Driver
import Test.Formlet.Field.Halogen as Test.Formlet.Field.Halogen
import Test.Formlet.Managed.Halogen as Test.Formlet.Managed.Halogen
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert
import Test.Unit.Main as Test.Unit.Main

main :: Effect Unit
main =
  Test.Unit.Main.runTest do
    suite
    Test.Formlet.Field.Halogen.suite
    Test.Formlet.Managed.Halogen.suite

-- TODO(arthur): unfortunately it seems like we cannot test for component
-- Outputs with our current implementation of `Halogen.Test.Driver`, as
-- subscribing to `HalogenIO`'s `messages` causes the test suite to fail without
-- any error messages. We should investigate and attempt to fix that in the
-- future and then implement the remaining test cases.
suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Test.Formlet.Halogen" do
    Test.Unit.test "`Validate` query should correctly validate the form" do
      let
        form ::
          forall config options renders.
          Formlet.Form config (Formlet.Render.Render (errors :: Formlet.Errors, required :: Boolean | options) (const :: Data.Const.Const String | renders)) Aff String String
        form =
          Formlet.Validation.validated (Formlet.Validation.mustEqual "Test" { error: "Invalid" })
            $ Formlet.form_ \_ -> Formlet.Render.inj <<< { const: _ } <<< Data.Const.Const
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Formlet.Halogen.component form (\_ -> Formlet.Render.match { const: Halogen.HTML.text <<< un Data.Const.Const }))
          { config: unit
          , value: ""
          }
      actual <- io.query (Formlet.Halogen.Validate identity)
      Test.Unit.Assert.equal (Just (Left [ "Invalid" ])) actual
      io' <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Formlet.Halogen.component form (\_ -> Formlet.Render.match { const: Halogen.HTML.text <<< un Data.Const.Const }))
          { config: unit
          , value: "Test"
          }
      actual' <- io'.query (Formlet.Halogen.Validate identity)
      Test.Unit.Assert.equal (Just (Right "Test")) actual'
