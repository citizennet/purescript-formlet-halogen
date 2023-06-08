module Form2.Render.Forest.Halogen
  ( render
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Data.Functor.Variant as Data.Functor.Variant
import Form2.Render.Forest as Form2.Render.Forest
import Form2.Render.Halogen as Form2.Render.Halogen
import Form2.Render.List as Form2.Render.List
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Elements.Keyed as Halogen.HTML.Elements.Keyed

-- | Transform a `Forest` render functor into HTML given a way of transforming
-- | each of the wrapped render functors and also a way of wrapping the
-- | resulting child HTML with the render options.
render ::
  forall slots config options renders m action.
  MonadAff m =>
  (Form2.Render.List.Key -> config -> Option options -> Array (Halogen.ComponentHTML action slots m) -> Halogen.ComponentHTML action slots m) ->
  (Form2.Render.List.Key -> config -> Data.Functor.Variant.VariantF renders action -> Array (Halogen.ComponentHTML action slots m)) ->
  config ->
  Form2.Render.Forest.Forest options renders action ->
  Halogen.ComponentHTML action slots m
render renderOptions renderElement config (Form2.Render.List.List list) =
  case Data.Array.uncons list of
    Nothing -> Halogen.HTML.text ""
    Just { head: { key, render: render' }, tail: [] } -> go key render'
    Just _ ->
      Halogen.HTML.Elements.Keyed.div_
        $ map (\{ key, render: render' } -> Tuple key (go key render'))
        $ list
  where
  go ::
    Form2.Render.List.Key ->
    Form2.Render.Forest.Tree options renders action ->
    Halogen.ComponentHTML action slots m
  go key = case _ of
    Form2.Render.Forest.Leaf l -> Form2.Render.Halogen.render (renderOptions key) (renderElement key) config l
    Form2.Render.Forest.Node n ->
      -- We don't just call `Form2.Render.List.Halogen.render` here because that
      -- render function is suited only for the top-level of forms, as it only
      -- wraps the list in a HTML container, and does not apply any render
      -- `options` as we do here.
      renderOptions key config n.options
        $ map (\{ key: key', render: render' } -> go key' render')
        $ un Form2.Render.List.List n.children
