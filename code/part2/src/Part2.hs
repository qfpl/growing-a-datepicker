{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Part2 (widg) where

import           Control.Lens                             ((.~))

import           Util.Attach

import qualified Reflex                                   as R

import           Reflex.Dom                               (MonadWidget)
import qualified Reflex.Dom                               as RD

import qualified Reflex.Dom.Widget.Input.Datepicker       as D
import qualified Reflex.Dom.Widget.Input.Datepicker.Types as D

import           Data.Function                            ((&))

import qualified Data.Text                                as Text
import qualified Data.Time                                as Time

import           GHCJS.DOM.Types                          (JSM)

-- Australian TimeLocale
aust :: Time.TimeLocale
aust = Time.defaultTimeLocale
  { Time.dateTimeFmt = Time.iso8601DateFormat (Just "%H:%M:%S")
  , Time.dateFmt = Time.iso8601DateFormat Nothing
  , Time.knownTimeZones =
    [ Time.TimeZone 600 False "AEST"
    , Time.TimeZone (9 * 60 + 30) False "ACST"
    , Time.TimeZone (8 * 60) False "AWST"
    ]
  }

fullSimpleDatepickerWidget
  :: MonadWidget t m
  => m ()
fullSimpleDatepickerWidget = do
  let
    showDate =
      Text.pack . Time.showGregorian

    cfg = D.simpleDateInputConfig aust
      & D.dateInputConfig_initialValue .~ Time.fromGregorian 2017 2 3

  -- Place the simple date picker on the page. This is a "prebaked" widget that
  -- has a lot of the functionality built into a single component with some flexible styling.
  dateIn <- D.datePickerSimple cfg

  dDaySelect <- R.holdDyn "No Day Clicked" $
    showDate <$> D._dateInput_daySelect dateIn

  dDate <- R.holdDyn "No Day Value" $
    showDate <$> ( R.updated $ D._dateInput_value dateIn )

  -- Show the last day that was clicked from the list of days for the last valid date value
  RD.el "h3" $
    RD.text "Date Selected: " >> RD.dynText dDaySelect

  -- Show the current stored valid day value
  RD.el "h3" $
    RD.text "Date Value: " >> RD.dynText dDate

widg
  :: JSM ()
widg =
  attachId_ "datepicker-simple" fullSimpleDatepickerWidget
