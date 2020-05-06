module AvailableTime.AvailableTime exposing (AvailableTimeDetails)

import Date exposing (Date)
import TimeSlots.TimeSlots as TS


type alias AvailableTimeDetails =
    { date : Date
    , startSlot : TS.SlotNum
    , endSlot : TS.SlotNum
    }
