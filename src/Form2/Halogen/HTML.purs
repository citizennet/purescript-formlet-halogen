module Form2.Halogen.HTML
  ( HTML(..)
  , html
  , htmlWithValue
  , render
  ) where

import CitizenNet.Prelude

import Form2 as Form2
import Form2.Render as Form2.Render
import Halogen.HTML as Halogen.HTML

-- | A Form2 render functor that contains plain Halogen HTML, that is, HTML
-- | which cannot contain child components nor affect the form state.
newtype HTML (action :: Type) =
  HTML Halogen.HTML.PlainHTML

derive instance newtypeHTML :: Newtype (HTML action) _

derive instance functorHTML :: Functor HTML

-- | Inject plain Halogen HTML into a `Form2.Form`.
html ::
  forall config options renders m value.
  Halogen.HTML.PlainHTML ->
  Form2.Form config (Form2.Render.Render options (html :: HTML | renders)) m value Unit
html plainHTML =
  void
    $ Form2.form_ \_ _ ->
        Form2.Render.inj
          { html: HTML plainHTML
          }

-- | Inject plain Halogen HTML into a `Form2.Form` given a function that
-- | produces Halogen HTML based on the form state.
htmlWithValue ::
  forall config options renders m value.
  (value -> Halogen.HTML.PlainHTML) ->
  Form2.Form config (Form2.Render.Render options (html :: HTML | renders)) m value value
htmlWithValue getHTML = Form2.withValue \value -> value <$ html (getHTML value)

-- | Render a `Form2.Halogen.HTML` functor back into plain Halogen HTML.
render ::
  forall action w i.
  HTML action ->
  Array (Halogen.HTML.HTML w i)
render (HTML plainHTML) = [ Halogen.HTML.fromPlainHTML plainHTML ]
