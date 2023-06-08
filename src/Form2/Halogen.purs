module Form2.Halogen
  ( Input
  , Output
  , Query(..)
  , Slot
  , component
  ) where

import CitizenNet.Prelude

import Data.Bifunctor as Data.Bifunctor
import Form2 as Form2
import Form2.Field.Halogen as Form2.Field.Halogen
import Halogen as Halogen

data Action config m value
  = Receive (Input config value)
  | UpdateValue (m (value -> value))

type Input config value =
  { config :: config
  , value :: value
  }

type State config render (m :: Type -> Type) value =
  { config :: config
  , render :: render (m (value -> value))
  , value :: value
  }

type Output value =
  value -> value

data Query result a
  = ClearErrors a
  | DisplayErrors a
  | GetErrors (Form2.Errors -> a)
  | Validate (Either (Array String) result -> a)

type Slot value result =
  Halogen.Slot (Query result) (Output value)

type Slots m value slots =
  Form2.Field.Halogen.Slots (m (value -> value)) slots

-- | A Halogen component that manages a Form's state and renders the Form to
-- | HTML with every subsequent change to the state given a way of transforming
-- | the Form's `render` functor into `ComponentHTML`.
component ::
  forall slots config render m value result.
  Functor render =>
  MonadAff m =>
  Form2.Form config render m value result ->
  (config -> render (m (value -> value)) -> Halogen.ComponentHTML (m (value -> value)) (Slots m value slots) m) ->
  Halogen.Component (Query result) (Input config value) (Output value) m
component form renderForm =
  Halogen.mkComponent
    { eval:
        Halogen.mkEval
          Halogen.defaultEval
            { handleAction = handleAction form
            , handleQuery = handleQuery form
            , receive = Just <<< Receive
            }
    , initialState:
        \{ config, value } ->
          { render: Form2.render form config value
          , config
          , value
          }
    , render: render renderForm
    }

handleAction ::
  forall slots config render m value result.
  Functor render =>
  MonadAff m =>
  Form2.Form config render m value result ->
  Action config m value ->
  Halogen.HalogenM (State config render m value) (Action config m value) (Slots m value slots) (Output value) m Unit
handleAction form = case _ of
  Receive { config, value } -> do
    Halogen.put
      { config
      , render: Form2.render form config value
      , value
      }
  UpdateValue getUpdate -> do
    Halogen.raise =<< Halogen.lift getUpdate

handleQuery ::
  forall slots config render m value result a.
  MonadAff m =>
  Functor render =>
  Form2.Form config render m value result ->
  Query result a ->
  Halogen.HalogenM (State config render m value) (Action config m value) (Slots m value slots) (Output value) m (Maybe a)
handleQuery form = case _ of
  ClearErrors done -> do
    _ <- Halogen.queryAll (Proxy :: Proxy "field") (Form2.Field.Halogen.ClearErrors unit)
    pure $ Just done
  DisplayErrors done -> do
    _ <- Halogen.queryAll (Proxy :: Proxy "field") (Form2.Field.Halogen.DisplayErrors unit)
    pure (Just done)
  GetErrors done -> do
    results <- Halogen.queryAll (Proxy :: Proxy "field") (Form2.Field.Halogen.GetErrors identity)
    pure $ Just (done (fold results))
  Validate done -> do
    _ <- Halogen.queryAll (Proxy :: Proxy "field") (Form2.Field.Halogen.DisplayErrors unit)
    { config, value } <- Halogen.get
    pure $ Just (done (Form2.validate form config value))

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
