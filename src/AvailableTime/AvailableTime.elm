module AvailableTime.AvailableTime exposing
    ( AvailableTimeDetails
    , ServerAvailableTimesCount
    , WithAlreadySubmittedConfirmedEvent
    , WithAvailabilityMap
    , WithAvailableTimesCount
    , isSlotAvailable
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


type alias WithAvailabilityMap a =
    { a
        | availabilityMap : List AvailableTimeDetails
    }


isSlotAvailable : WithAvailabilityMap a -> Date -> TS.SlotNum -> Bool
isSlotAvailable model slotDate slotNum =
    let
        containsSlot { date, startSlot, endSlot } =
            slotDate == date && slotNum >= startSlot && slotNum <= endSlot
    in
    List.any containsSlot model.availabilityMap


type alias WithAlreadySubmittedConfirmedEvent a =
    { a
        | alreadySubmittedConfirmedEvent : Bool
    }
