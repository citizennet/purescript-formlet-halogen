module Test.Form2.Field.Halogen
  ( suite
  ) where

import CitizenNet.Prelude

import Debug as Debug
import Effect.Aff as Effect.Aff
import Form2 as Form2
import Form2.Field.Halogen as Form2.Field.Halogen
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.Subscription as Halogen.Subscription
import Halogen.Test.Driver as Halogen.Test.Driver
import Halogen.Test.Subscription as Halogen.Test.Subscription
import Test.Unit as Test.Unit
import Test.Utils as Test.Utils

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Form2.Field.Halogen" do
    Test.Unit.test "An output in the child render should trigger displaying validation errors" do
      { emitter, listener } <- liftEffect Halogen.Subscription.create
      { emitter: testEmitter, listener: testListener } <- liftEffect Halogen.Subscription.create
      let
        errors :: Form2.Errors
        errors = [ "Some errors" ]
      -- In this test we use a Halogen subscription to send an `Aff` test
      -- inside a component's rendering context. This way we can have access to
      -- any values that only exist inside that rendering context when
      -- performing test assertions.
      _ <-
        runTestFieldUI errors \mErrors ->
          Halogen.HTML.div_
            -- Here we listen for any events in `emitter`, which we then raise
            -- as output, effectively causing the field to be touched.
            [ Halogen.Test.Subscription.subscribe
                (emitter :: Halogen.Subscription.Emitter Unit)
                Halogen.raise
            , Halogen.Test.Subscription.subscribe
                ( map (_ $ mErrors)
                    (testEmitter :: Halogen.Subscription.Emitter (Maybe Form2.Errors -> Aff Unit))
                )
                liftAff
            ]
      -- Before a field is touched, no validation errors should be rendered.
      testInListener testListener Nothing
      -- Here we issue an event to `listener`, causing field to be touched.
      liftEffect $ Halogen.Subscription.notify listener unit
      -- We need some delay here to wait for the delay in
      -- `Form2.Field.Halogen`'s handling of child outputs.
      Effect.Aff.delay (Effect.Aff.Milliseconds 50.0)
      -- Validation errors should be rendered now that the field is touched.
      testInListener testListener (Just errors)
    Test.Unit.test "`ClearErrors` query should clear errors on all child `Form2.Field.Halogen` components" do
      { emitter, listener } <- liftEffect Halogen.Subscription.create
      let
        errors :: Form2.Errors
        errors = [ "Some errors" ]
      -- In this test we use a Halogen subscription to send an `Aff` test
      -- inside a component's rendering context. This way we can have access to
      -- any values that only exist inside that rendering context when
      -- performing test assertions.
      io <-
        runTestFieldUI errors \mErrors ->
          Halogen.Test.Subscription.subscribe
            (map (_ $ mErrors) (emitter :: Halogen.Subscription.Emitter (Maybe Form2.Errors -> Aff Unit)))
            liftAff
      -- We want to test whether `ClearErrors` really hides any validation
      -- errors, so first we must display them.
      _ <- io.query (Form2.Field.Halogen.DisplayErrors unit)
      -- We check whether they're visible before we clear them.
      testInListener listener (Just errors)
      _ <- io.query (Form2.Field.Halogen.ClearErrors unit)
      -- Validation errors should be rendered now that the `ClearErrors` query
      -- was issued and the field is now considered being untouched.
      testInListener listener Nothing
    Test.Unit.test "`DisplayErrors` query should display errors on all child `Form2.Field.Halogen` components" do
      { emitter, listener } <- liftEffect Halogen.Subscription.create
      let
        errors :: Form2.Errors
        errors = [ "Some errors" ]
      -- In this test we use a Halogen subscription to send an `Aff` test
      -- inside a component's rendering context. This way we can have access to
      -- any values that only exist inside that rendering context when
      -- performing test assertions.
      io <-
        runTestFieldUI errors \mErrors ->
          Halogen.Test.Subscription.subscribe
            (map (_ $ mErrors) (emitter :: Halogen.Subscription.Emitter (Maybe Form2.Errors -> Aff Unit)))
            liftAff
      -- When a field is first initialized, no validation errors should be
      -- rendered.
      testInListener listener Nothing
      _ <- io.query (Form2.Field.Halogen.DisplayErrors unit)
      -- Validation errors should be rendered now that the `DisplayErrors` query
      -- was issued and the field is now considered being touched.
      testInListener listener (Just errors)

runTestFieldUI ::
  forall output slots.
  Form2.Errors ->
  (Maybe Form2.Errors -> Halogen.ComponentHTML output (Form2.Field.Halogen.Slots output slots) Aff) ->
  Aff (Halogen.HalogenIO Form2.Field.Halogen.Query output Aff)
runTestFieldUI errors render =
  Halogen.Test.Driver.runUI
    { duplicateSlot: mempty }
    Form2.Field.Halogen.component
    { errors: Just errors
    , render:
        \_ ->
          Halogen.HTML.slot
            (Proxy :: Proxy "field")
            "someFieldKey"
            Form2.Field.Halogen.component
            { errors: Just errors
            , render: render
            }
            identity
    }

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
