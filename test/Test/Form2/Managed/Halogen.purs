module Test.Form2.Managed.Halogen
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Const as Data.Const
import Debug as Debug
import Form2 as Form2
import Form2.Field.Halogen as Form2.Field.Halogen
import Form2.Managed.Halogen as Form2.Managed.Halogen
import Form2.Render as Form2.Render
import Form2.Validation as Form2.Validation
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.Subscription as Halogen.Subscription
import Halogen.Test.Driver as Halogen.Test.Driver
import Halogen.Test.Subscription as Halogen.Test.Subscription
import Test.Unit as Test.Unit
import Test.Utils as Test.Utils

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Test.Form2.Managed.Halogen" do
    Test.Unit.test "Input `config` should be available in the rendering" do
      { emitter, listener } <- liftEffect Halogen.Subscription.create
      let
        -- The `subscribe` component is mounted in the render and subscribes to
        -- the `emitter` created above. We then notify the `listener` of update
        -- actions of type `Aff (String -> String)`, which are captured by
        -- `subscribe` and raised as output, which is then captured by
        -- `Form2.Managed.Halogen.component`, which triggers a form state update.
        form :: forall slots. Form2.Form String (Halogen.Test.Subscription.HTML Aff (Aff (String -> String)) slots) Aff String String
        form =
          Form2.form_ \config _ ->
            Halogen.Test.Subscription.subscribe emitter \_ -> Halogen.raise (pure (\_ -> config))
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Form2.Managed.Halogen.component form (\_ -> identity))
          { config: "test"
          , initialValue: ""
          }
      testValue io ""
      liftEffect $ Halogen.Subscription.notify listener (pure identity :: Aff (String -> String))
      testValue io "test"
    Test.Unit.test "Initial `value` should be available in the rendering" do
      { emitter, listener } <- liftEffect Halogen.Subscription.create
      let
        form ::
          forall config slots.
          Form2.Form config (Halogen.Test.Subscription.HTML Aff (Aff (String -> String)) slots) Aff String String
        form = Form2.form_ \_ _ -> Halogen.Test.Subscription.subscribe emitter Halogen.raise
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Form2.Managed.Halogen.component form (\_ -> identity))
          { config: unit
          , initialValue: "test"
          }
      testValue io "test"
      liftEffect $ Halogen.Subscription.notify listener (pure (_ <> "1"))
      testValue io "test1"
    Test.Unit.test "Value changes should be fed back to the render" do
      { emitter, listener } <- liftEffect Halogen.Subscription.create
      let
        -- In this test we modify every update action to set `previous` to the
        -- value captured in the render closure, that is, the value we have
        -- before applying the update.
        form ::
          forall config slots.
          Form2.Form config (Halogen.Test.Subscription.HTML Aff (Aff (Change Int -> Change Int)) slots) Aff (Change Int) (Change Int)
        form =
          Form2.form_ \_ value ->
            Halogen.Test.Subscription.subscribe
              (map (map (\update -> _ { previous = value.current } <<< update)) emitter)
              Halogen.raise
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Form2.Managed.Halogen.component form (\_ -> identity))
          { config: unit
          , initialValue: { current: 1, previous: 0 }
          }
      testValue io { current: 1, previous: 0 }
      liftEffect $ Halogen.Subscription.notify listener (pure _ { current = 2 } :: Aff _)
      testValue io { current: 2, previous: 1 }
      liftEffect $ Halogen.Subscription.notify listener (pure _ { current = 3 })
      testValue io { current: 3, previous: 2 }
    Test.Unit.test "`ClearErrors` query should clear errors on all child `Form2.Field.Halogen` components" do
      { emitter, listener } <- liftEffect Halogen.Subscription.create
      let
        errors :: Form2.Errors
        errors = [ "Some errors" ]
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          ( Form2.Managed.Halogen.component constForm \_ _ ->
              -- In this test we use a Halogen subscription to send an `Aff`
              -- test inside a component's rendering context. This way we can
              -- have access to any values that only exist inside that rendering
              -- context when performing test assertions.
              Halogen.HTML.slot
                (Proxy :: Proxy "field")
                "someFieldKey"
                Form2.Field.Halogen.component
                { errors: Just errors
                , render: \mErrors -> Halogen.Test.Subscription.subscribe (map (_ $ mErrors) emitter) liftAff
                }
                identity
          )
          { config: unit
          , initialValue: "test"
          }
      _ <- io.query (Form2.Managed.Halogen.DisplayErrors unit)
      testInListener listener (Just errors)
      _ <- io.query (Form2.Managed.Halogen.ClearErrors unit)
      testInListener listener Nothing
    Test.Unit.test "`DisplayErrors` query should display errors on all child `Form2.Field.Halogen` components" do
      { emitter, listener } <- liftEffect Halogen.Subscription.create
      let
        errors :: Form2.Errors
        errors = [ "Some errors" ]
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          ( Form2.Managed.Halogen.component constForm \_ _ ->
              -- In this test we use a Halogen subscription to send an `Aff`
              -- test inside a component's rendering context. This way we can
              -- have access to any values that only exist inside that rendering
              -- context when performing test assertions.
              Halogen.HTML.slot
                (Proxy :: Proxy "field")
                "someFieldKey"
                Form2.Field.Halogen.component
                { errors: Just errors
                , render: \mErrors -> Halogen.Test.Subscription.subscribe (map (_ $ mErrors) emitter) liftAff
                }
                identity
          )
          { config: unit
          , initialValue: "test"
          }
      testInListener listener Nothing
      _ <- io.query (Form2.Managed.Halogen.DisplayErrors unit)
      testInListener listener (Just errors)
    Test.Unit.test "`SetValue` query should update the form value" do
      let
        form :: forall config. Form2.Form config (Data.Const.Const String) Aff String String
        form = Form2.form_ \_ -> Data.Const.Const
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Form2.Managed.Halogen.component form (\_ -> Halogen.HTML.text <<< un Data.Const.Const))
          { config: unit
          , initialValue: "test"
          }
      testValue io "test"
      _ <- io.query (Form2.Managed.Halogen.SetValue "test1" unit)
      testValue io "test1"
    Test.Unit.test "`Validate` query should correctly validate the form" do
      let
        form ::
          forall config options renders.
          Form2.Form config (Form2.Render.Render (errors :: Form2.Errors, required :: Boolean | options) (const :: Data.Const.Const String | renders)) Aff String String
        form =
          Form2.Validation.validated (Form2.Validation.mustEqual "Test" { error: "Invalid" })
            $ Form2.mapRender (Form2.Render.inj <<< { const: _ })
            $ constForm
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Form2.Managed.Halogen.component form (\_ -> Form2.Render.match { const: Halogen.HTML.text <<< un Data.Const.Const }))
          { config: unit
          , initialValue: ""
          }
      actual <- io.query (Form2.Managed.Halogen.Validate identity)
      Test.Utils.equal (Just (Left [ "Invalid" ])) actual
      io' <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Form2.Managed.Halogen.component form (\_ -> Form2.Render.match { const: Halogen.HTML.text <<< un Data.Const.Const }))
          { config: unit
          , initialValue: "Test"
          }
      actual' <- io'.query (Form2.Managed.Halogen.Validate identity)
      Test.Utils.equal (Just (Right "Test")) actual'
    Test.Unit.test "`Validate` query should display errors on all child `Form2.Field.Halogen` components" do
      { emitter, listener } <- liftEffect Halogen.Subscription.create
      let
        form ::
          forall config options renders.
          Form2.Form config (Form2.Render.Render (errors :: Form2.Errors, required :: Boolean | options) (const :: Data.Const.Const String | renders)) Aff String String
        form =
          Form2.Validation.validated (Form2.Validation.mustEqual "Test" { error: "Invalid" })
            $ Form2.mapRender (Form2.Render.inj <<< { const: _ })
            $ constForm

        errors :: Form2.Errors
        errors = [ "Some errors" ]
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          ( Form2.Managed.Halogen.component form \_ _ ->
              -- In this test we use a Halogen subscription to send an `Aff`
              -- test inside a component's rendering context. This way we can
              -- have access to any values that only exist inside that rendering
              -- context when performing test assertions.
              Halogen.HTML.slot
                (Proxy :: Proxy "field")
                "someFieldKey"
                Form2.Field.Halogen.component
                { errors: Just errors
                , render: \mErrors -> Halogen.Test.Subscription.subscribe (map (_ $ mErrors) emitter) liftAff
                }
                identity
          )
          { config: unit
          , initialValue: "test"
          }
      testInListener listener Nothing
      _ <- io.query (Form2.Managed.Halogen.Validate identity)
      testInListener listener (Just errors)

-----------
-- Internal
-----------
-- This type is used in the form state of the feedback test
type Change a =
  { current :: a
  , previous :: a
  }

constForm ::
  forall config m value.
  Form2.Form config (Data.Const.Const value) m value value
constForm = Form2.form_ \_ -> Data.Const.Const

-- | Utility function for sending a test assertion to a
-- | `Halogen.Subscription.Listener`. This is useful when we want to test a
-- | value in a context other than a `Test`, where a `Listener` can be used.
testInListener ::
  forall value.
  Eq value =>
  Debug.Debug value =>
  Halogen.Subscription.Listener (value -> Aff Unit) ->
  value ->
  Aff Unit
testInListener listener = liftEffect <<< Halogen.Subscription.notify listener <<< Test.Utils.equal

-- | Utility function for testing the internal value of a
-- | `Form2.Managed.Halogen` component.
testValue ::
  forall output result value.
  Eq value =>
  Debug.Debug value =>
  Halogen.HalogenIO (Form2.Managed.Halogen.Query value result) output Aff ->
  value ->
  Aff Unit
testValue io expected = do
  actual <- io.query (Form2.Managed.Halogen.GetValue identity)
  Test.Utils.equal (Just expected) actual
