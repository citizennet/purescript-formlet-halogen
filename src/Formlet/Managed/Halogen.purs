module Formlet.Managed.Halogen
  ( Component
  , Input
  , Output
  , Query(..)
  , Slot
  , Slots
  , component
  ) where

import CitizenNet.Prelude

import Data.Bifunctor as Data.Bifunctor
import Formlet as Formlet
import Formlet.Field.Halogen as Formlet.Field.Halogen
import Halogen as Halogen

data Action config m value
  = ReceiveConfig config
  | UpdateValue (m (value -> value))

type Component config m value result =
  Halogen.Component (Query value result) (Input config value) (Output value) m

type Input config value =
  { config :: config
  , initialValue :: value
  }

type Output value =
  value -> value

data Query value result a
  = ClearErrors a
  | DisplayErrors a
  | GetErrors (Formlet.Errors -> a)
  | GetValue (value -> a)
  | SetValue value a
  | Validate (Either (Array String) result -> a)

type Slot value result =
  Halogen.Slot (Query value result) (Output value)

type Slots m value slots =
  Formlet.Field.Halogen.Slots (m (value -> value)) slots

type State config render (m :: Type -> Type) value =
  { config :: config
  , render :: render (m (value -> value))
  , value :: value
  }

-- | A Halogen component that manages a Form's state and renders the Form to
-- | HTML with every subsequent change to the state given a way of transforming
-- | the Form's `render` functor into `ComponentHTML`.
component ::
  forall slots config render m value result.
  Functor render =>
  MonadAff m =>
  Formlet.Form config render m value result ->
  (config -> render (m (value -> value)) -> Halogen.ComponentHTML (m (value -> value)) (Slots m value slots) m) ->
  Component config m value result
component form renderForm =
  Halogen.mkComponent
    { eval:
        Halogen.mkEval
          Halogen.defaultEval
            { handleAction = handleAction form
            , handleQuery = handleQuery form
            , receive = Just <<< ReceiveConfig <<< _.config
            }
    , initialState:
        \{ config, initialValue } ->
          { render: Formlet.render form config initialValue
          , config
          , value: initialValue
          }
    , render: render renderForm
    }

handleAction ::
  forall slots config render m value result.
  Functor render =>
  MonadAff m =>
  Formlet.Form config render m value result ->
  Action config m value ->
  Halogen.HalogenM (State config render m value) (Action config m value) (Slots m value slots) (Output value) m Unit
handleAction form = case _ of
  ReceiveConfig config -> do
    Halogen.modify_ \state -> state { render = Formlet.render form config state.value, config = config }
  UpdateValue getUpdate -> do
    update <- Halogen.lift getUpdate
    Halogen.modify_ \state ->
      let
        value' = update state.value
      in
        state { render = Formlet.render form state.config value', value = value' }
    Halogen.raise update

handleQuery ::
  forall slots config render m value result a.
  MonadAff m =>
  Functor render =>
  Formlet.Form config render m value result ->
  Query value result a ->
  Halogen.HalogenM (State config render m value) (Action config m value) (Slots m value slots) (Output value) m (Maybe a)
handleQuery form = case _ of
  ClearErrors done -> do
    _ <- Halogen.queryAll (Proxy :: Proxy "field") (Formlet.Field.Halogen.ClearErrors unit)
    pure $ Just done
  DisplayErrors done -> do
    _ <- Halogen.queryAll (Proxy :: Proxy "field") (Formlet.Field.Halogen.DisplayErrors unit)
    pure $ Just done
  GetErrors done -> do
    results <- Halogen.queryAll (Proxy :: Proxy "field") (Formlet.Field.Halogen.GetErrors identity)
    pure $ Just (done (fold results))
  GetValue done -> do
    { value } <- Halogen.get
    pure $ Just (done value)
  SetValue value done -> do
    Halogen.modify_ \state ->
      state
        { render = Formlet.render form state.config value
        , value = value
        }
    pure $ Just done
  Validate done -> do
    _ <- Halogen.queryAll (Proxy :: Proxy "field") (Formlet.Field.Halogen.DisplayErrors unit)
    { config, value } <- Halogen.get
    pure $ Just (done (Formlet.validate form config value))

render ::
  forall slots config render m value.
  MonadAff m =>
  (config -> render (m (value -> value)) -> Halogen.ComponentHTML (m (value -> value)) (Slots m value slots) m) ->
  State config render m value ->
  Halogen.ComponentHTML (Action config m value) (Slots m value slots) m
render renderForm state =
  Data.Bifunctor.bimap (map UpdateValue) UpdateValue
    $ renderForm state.config
    $ state.render
