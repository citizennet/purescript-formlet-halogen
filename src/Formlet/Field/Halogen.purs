module Formlet.Field.Halogen
  ( Input
  , Query(..)
  , Slot
  , Slots
  , component
  ) where

import CitizenNet.Prelude

import Data.Bifunctor as Data.Bifunctor
import Data.Monoid as Data.Monoid
import Effect.Aff as Effect.Aff
import Formlet as Formlet
import Formlet.Render.List as Formlet.Render.List
import Halogen as Halogen

data Action m output slots
  = HandleOutput output
  | HandleReceive (Input m output slots)

type Input m output slots =
  { errors :: Maybe Formlet.Errors
  , render :: Maybe Formlet.Errors -> Halogen.ComponentHTML output (Slots output slots) m
  }

type Slot output =
  Halogen.Slot Query output

type Slots output slots =
  ( field :: Slot output Formlet.Render.List.Key
  | slots
  )

type State m output slots =
  { errors :: Maybe Formlet.Errors
  , render :: Maybe Formlet.Errors -> Halogen.ComponentHTML output (Slots output slots) m
  , touched :: Boolean
  }

data Query a
  = ClearErrors a
  | DisplayErrors a
  | GetErrors (Formlet.Errors -> a)

-- This component serves two main purposes:
--
-- 1. Encapsulating any slots that appear in the `render` function so that the
--    parent component doesn't have to deal with any of these child slots. This
--    helps with duplicated slots issues, as we only have to index this 'field'
--    component, and whatever components go in the `render` function can be
--    indexed by `Unit` (so long as they are unique).
-- 2. Managing the display of validation errors: the `render` function has an
--    argument of type `Maybe errors`, whose availability is controlled by this
--    very component. Whenever any outputs are captured from the HTML rendered
--    in the `render` function, this component starts passing the `errors` input
--    through to `render`, thus, in practice, this means that any changes to a
--    form in `render` trigger the display of validation errors.
component ::
  forall m output slots.
  MonadAff m =>
  Halogen.Component Query (Input m output slots) output m
component =
  Halogen.mkComponent
    { eval:
        Halogen.mkEval
          Halogen.defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            , receive = Just <<< HandleReceive
            }
    , initialState
    , render
    }

handleAction ::
  forall m output slots.
  MonadAff m =>
  Action m output slots ->
  Halogen.HalogenM (State m output slots) (Action m output slots) (Slots output slots) output m Unit
handleAction = case _ of
  HandleOutput output -> do
    -- Here we first raise the captured output so that any parent components can
    -- handle the form state update, then we set a 0ms delay so that displaying
    -- validation errors only happens in the next event cycle. This is needed
    -- because in our Halogen implementation, form state updates are usually
    -- triggered on `blur` (focus lost) events for some input types, whereas for
    -- some other input types, they're triggered on `click` events. In the case
    -- where we have two simultaneous interactions, say a text field becoming
    -- invalid after losing focus and a checkbox getting clicked, then the
    -- checkbox click may not be properly registered, as first the textbox loses
    -- focus, which triggers a form state update and shows its validation
    -- errors, which ends up displacing the checkbox, causing the click to not
    -- hit the checkbox element. We avoid this weird interaction by delaying the
    -- display of validation errors to the next event cycle after the
    -- interaction that triggers it.
    Halogen.raise output
    liftAff $ Effect.Aff.delay (Effect.Aff.Milliseconds 0.0)
    Halogen.modify_ _ { touched = true }
  HandleReceive input -> do
    Halogen.modify_
      _
        { errors = input.errors
        , render = input.render
        }

handleQuery ::
  forall a m output slots.
  Query a ->
  Halogen.HalogenM (State m output slots) (Action m output slots) (Slots output slots) output m (Maybe a)
handleQuery = case _ of
  ClearErrors done -> do
    Halogen.modify_ _ { touched = false }
    _ <- Halogen.queryAll (symbol { field: _ }) (ClearErrors unit)
    pure (Just done)
  DisplayErrors done -> do
    Halogen.modify_ _ { touched = true }
    _ <- Halogen.queryAll (symbol { field: _ }) (DisplayErrors unit)
    pure (Just done)
  GetErrors done -> do
    state <- Halogen.get
    results <- Halogen.queryAll (symbol { field: _ }) (GetErrors identity)
    pure $ Just (done (foldMap (Data.Monoid.guard state.touched) state.errors <> fold results))

initialState ::
  forall m output slots.
  Input m output slots ->
  State m output slots
initialState input =
  { touched: false
  , errors: input.errors
  , render: input.render
  }

render ::
  forall m output slots.
  State m output slots ->
  Halogen.ComponentHTML (Action m output slots) (Slots output slots) m
render state =
  Data.Bifunctor.bimap (map HandleOutput) HandleOutput
    $ state.render
    $ if state.touched then state.errors else Nothing
