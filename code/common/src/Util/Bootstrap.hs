{-# LANGUAGE OverloadedStrings #-}
module Util.Bootstrap (
    panel
  , button
  , buttonClass
  ) where

import Data.Monoid ((<>))

import Data.Text (Text)
import Reflex.Dom.Core hiding (button)

import qualified Util.Reflex as R

button ::
  DomBuilder t m =>
  Text ->
  m (Event t ())
button =
  R.buttonClass "btn btn-default"

buttonClass ::
  DomBuilder t m =>
  Text ->
  Text ->
  m (Event t ())
buttonClass cl =
  R.buttonClass ("btn btn-default " <> cl)

panel ::
  DomBuilder t m =>
  m a ->
  m a
panel =
  divClass "panel panel-default" .
  divClass "panel-body"
