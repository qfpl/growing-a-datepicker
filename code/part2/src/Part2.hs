{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Part2 (widg) where

import           Control.Lens                             ((.~))
import           Data.Semigroup                           ((<>))

import           Util.Attach

import qualified Reflex                                   as R

import           Reflex.Dom                               (MonadWidget, (=:))
import qualified Reflex.Dom                               as RD

import qualified Reflex.Dom.Widget.Input.Datepicker       as D
import qualified Reflex.Dom.Widget.Input.Datepicker.Style as D
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

wrapDatePicker
  :: MonadWidget t m => D.Wrap D.DatePickerW t m
wrapDatePicker = D.Wrap $
  RD.divClass "container-fluid"
  . RD.divClass "row"
    . RD.divClass "col-xs-12"

wrapControls
  :: MonadWidget t m => D.Wrap D.ControlW t m
wrapControls = D.Wrap $
  RD.divClass "form-inline text-center"

wrapDayList
  :: MonadWidget t m => D.Wrap D.DayListW t m
wrapDayList = D.Wrap $
  RD.divClass "panel panel-default text-center"
  . RD.divClass "panel-body"

wrapMonthButton
  :: MonadWidget t m => D.Wrap D.MonthBtnW t m
wrapMonthButton = D.Wrap $
  RD.divClass "form-group month-button-wrap"

fullSimpleDatepickerWidget
  :: MonadWidget t m
  => m ()
fullSimpleDatepickerWidget = RD.divClass "container" $ do
  let
    showDate =
      Text.pack . Time.showGregorian

    cfg = D.simpleDateInputConfig aust
      & D.dateInputConfig_initialValue .~ Time.fromGregorian 2017 2 3

      & D.dateInputConfig_textInputAttrs .~
        pure ( "class" =: "form-control datepicker-text-input" )

      & D.dateInputConfig_mthBtnAttrs .~
        pure ( "class" =: "btn btn-default" )

      & D.dateInputConfig_dayAttrs .~
        pure (\selected ->
                 "class" =: (
                   -- Add the 'active' class when we're selected
                   "label " <> if selected
                               then "label-warning"
                               else "label-info"
                   )
             )

  -- Place the simple date picker on the page. This is a "prebaked" widget that
  -- has a lot of the functionality built into a single component with some flexible styling.
  dateIn <- RD.divClass "col-xs-12 col-md-4"
    . RD.divClass "datepicker-widget" $
      D.datePickerWrappedWith wrapDatePicker wrapControls wrapMonthButton D.dayElWrap wrapDayList cfg

  dDaySelect <- R.holdDyn "No Day Clicked" $
    showDate <$> D._dateInput_daySelect dateIn

  dDate <- R.holdDyn "No Day Value" $
    showDate <$> ( R.updated $ D._dateInput_value dateIn )

  RD.divClass "col-xs-12 col-md-4" .
    RD.divClass "panel panel-default"
    . RD.divClass "panel-body text-center" $
    do
      -- Show the last day that was clicked from the list of days for the last valid date value
      RD.el "h4" (RD.text "Date Selected: " >> RD.dynText dDaySelect)
      -- Show the current stored valid day value
      RD.el "h4" (RD.text "Date Value: " >> RD.dynText dDate)

widg
  :: JSM ()
widg = attachId_
  "datepicker-simple"
  fullSimpleDatepickerWidget
