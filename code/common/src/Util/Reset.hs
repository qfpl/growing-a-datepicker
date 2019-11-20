{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Util.Reset (
    reset
  ) where

import Control.Monad.Fix (MonadFix)
import Reflex.Dom.Core

import qualified Util.Bootstrap as B

reset ::
  (DomBuilder t m, MonadHold t m, MonadFix m) =>
  m a ->
  m ()
reset w = mdo
  _ <- widgetHold w (w <$ eReset)
  eReset <- el "div" $
    B.buttonClass "pull-right" "Reset"
  pure ()
