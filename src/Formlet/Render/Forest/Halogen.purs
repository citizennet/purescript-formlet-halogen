module Formlet.Render.Forest.Halogen
  ( render
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Data.Functor.Variant as Data.Functor.Variant
import Formlet.Render.Forest as Formlet.Render.Forest
import Formlet.Render.Halogen as Formlet.Render.Halogen
import Formlet.Render.List as Formlet.Render.List
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Elements.Keyed as Halogen.HTML.Elements.Keyed

-- | Transform a `Forest` render functor into HTML given a way of transforming
-- | each of the wrapped render functors and also a way of wrapping the
-- | resulting child HTML with the render options.
render ::
  forall slots config options renders m action.
  MonadAff m =>
  (Formlet.Render.List.Key -> config -> Option options -> Array (Halogen.ComponentHTML action slots m) -> Halogen.ComponentHTML action slots m) ->
  (Formlet.Render.List.Key -> config -> Data.Functor.Variant.VariantF renders action -> Array (Halogen.ComponentHTML action slots m)) ->
  config ->
  Formlet.Render.Forest.Forest options renders action ->
  Halogen.ComponentHTML action slots m
render renderOptions renderElement config (Formlet.Render.List.List list) =
  case Data.Array.uncons list of
    Nothing -> Halogen.HTML.text ""
    Just { head: { key, render: render' }, tail: [] } -> go key render'
    Just _ ->
      Halogen.HTML.Elements.Keyed.div_
        $ map (\{ key, render: render' } -> Tuple key (go key render'))
        $ list
  where
  go ::
    Formlet.Render.List.Key ->
    Formlet.Render.Forest.Tree options renders action ->
    Halogen.ComponentHTML action slots m
  go key = case _ of
    Formlet.Render.Forest.Leaf l -> Formlet.Render.Halogen.render (renderOptions key) (renderElement key) config l
    Formlet.Render.Forest.Node n ->
      -- We don't just call `Formlet.Render.List.Halogen.render` here because that
      -- render function is suited only for the top-level of forms, as it only
      -- wraps the list in a HTML container, and does not apply any render
      -- `options` as we do here.
      renderOptions key config n.options
        $ map (\{ key: key', render: render' } -> go key' render')
        $ un Formlet.Render.List.List n.children
