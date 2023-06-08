module Test.Form2.Halogen
  ( main
  ) where

import CitizenNet.Prelude

import Data.Const as Data.Const
import Form2 as Form2
import Form2.Halogen as Form2.Halogen
import Form2.Render as Form2.Render
import Form2.Validation as Form2.Validation
import Halogen.HTML as Halogen.HTML
import Halogen.Test.Driver as Halogen.Test.Driver
import Test.Form2.Field.Halogen as Test.Form2.Field.Halogen
import Test.Form2.Managed.Halogen as Test.Form2.Managed.Halogen
import Test.Unit as Test.Unit
import Test.Unit.Main as Test.Unit.Main
import Test.Utils as Test.Utils

main :: Effect Unit
main =
  Test.Unit.Main.runTest do
    suite
    Test.Form2.Field.Halogen.suite
    Test.Form2.Managed.Halogen.suite

-- TODO(arthur): unfortunately it seems like we cannot test for component
-- Outputs with our current implementation of `Halogen.Test.Driver`, as
-- subscribing to `HalogenIO`'s `messages` causes the test suite to fail without
-- any error messages. We should investigate and attempt to fix that in the
-- future and then implement the remaining test cases.
suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Test.Form2.Halogen" do
    Test.Unit.test "`Validate` query should correctly validate the form" do
      let
        form ::
          forall config options renders.
          Form2.Form config (Form2.Render.Render (errors :: Form2.Errors, required :: Boolean | options) (const :: Data.Const.Const String | renders)) Aff String String
        form =
          Form2.Validation.validated (Form2.Validation.mustEqual "Test" { error: "Invalid" })
            $ Form2.form_ \_ -> Form2.Render.inj <<< { const: _ } <<< Data.Const.Const
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Form2.Halogen.component form (\_ -> Form2.Render.match { const: Halogen.HTML.text <<< un Data.Const.Const }))
          { config: unit
          , value: ""
          }
      actual <- io.query (Form2.Halogen.Validate identity)
      Test.Utils.equal (Just (Left [ "Invalid" ])) actual
      io' <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Form2.Halogen.component form (\_ -> Form2.Render.match { const: Halogen.HTML.text <<< un Data.Const.Const }))
          { config: unit
          , value: "Test"
          }
      actual' <- io'.query (Form2.Halogen.Validate identity)
      Test.Utils.equal (Just (Right "Test")) actual'
