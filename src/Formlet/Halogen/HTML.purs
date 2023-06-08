module Formlet.Halogen.HTML
  ( HTML(..)
  , html
  , htmlWithValue
  , render
  ) where

import CitizenNet.Prelude

import Formlet as Formlet
import Formlet.Render as Formlet.Render
import Halogen.HTML as Halogen.HTML

-- | A Formlet render functor that contains plain Halogen HTML, that is, HTML
-- | which cannot contain child components nor affect the form state.
newtype HTML (action :: Type) =
  HTML Halogen.HTML.PlainHTML

derive instance newtypeHTML :: Newtype (HTML action) _

derive instance functorHTML :: Functor HTML

-- | Inject plain Halogen HTML into a `Formlet.Form`.
html ::
  forall config options renders m value.
  Halogen.HTML.PlainHTML ->
  Formlet.Form config (Formlet.Render.Render options (html :: HTML | renders)) m value Unit
html plainHTML =
  void
    $ Formlet.form_ \_ _ ->
        Formlet.Render.inj
          { html: HTML plainHTML
          }

-- | Inject plain Halogen HTML into a `Formlet.Form` given a function that
-- | produces Halogen HTML based on the form state.
htmlWithValue ::
  forall config options renders m value.
  (value -> Halogen.HTML.PlainHTML) ->
  Formlet.Form config (Formlet.Render.Render options (html :: HTML | renders)) m value value
htmlWithValue getHTML = Formlet.withValue \value -> value <$ html (getHTML value)

-- | Render a `Formlet.Halogen.HTML` functor back into plain Halogen HTML.
render ::
  forall action w i.
  HTML action ->
  Array (Halogen.HTML.HTML w i)
render (HTML plainHTML) = [ Halogen.HTML.fromPlainHTML plainHTML ]
