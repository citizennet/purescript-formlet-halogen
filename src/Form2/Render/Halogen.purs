module Formlet.Render.Halogen
  ( render
  ) where

import CitizenNet.Prelude

import Data.Functor.Variant as Data.Functor.Variant
import Formlet.Render as Formlet.Render
import Halogen as Halogen

-- | Transform the canonical `Render` functor into component HTML given a way of
-- | transforming the wrapped render functor and also a way of wrapping the
-- | resulting child HTML with the render options.
render ::
  forall slots config options renders m action.
  MonadAff m =>
  (config -> Option options -> Array (Halogen.ComponentHTML action slots m) -> Halogen.ComponentHTML action slots m) ->
  (config -> Data.Functor.Variant.VariantF renders action -> Array (Halogen.ComponentHTML action slots m)) ->
  config ->
  Formlet.Render.Render options renders action ->
  Halogen.ComponentHTML action slots m
render renderOptions renderElement config (Formlet.Render.Render r) =
  renderOptions config r.options
    $ renderElement config
    $ r.render
