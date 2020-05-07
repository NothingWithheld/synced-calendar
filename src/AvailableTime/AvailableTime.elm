module AvailableTime.AvailableTime exposing
    ( AvailableTimeDetails
    , ServerAvailableTimesCount
    , WithAvailableTimesCount
    )

import Date exposing (Date)
import TimeSlots.TimeSlots as TS


type alias AvailableTimeDetails =
    { date : Date
    , startSlot : TS.SlotNum
    , endSlot : TS.SlotNum
    }


type alias ServerAvailableTimesCount =
    { totalRecipients : Int
    , countSubmitted : Int
    }


type alias WithAvailableTimesCount a =
    { a
        | totalRecipients : Int
        , countSubmitted : Int
    }
