module TimeSlots.Commands exposing
    ( requestSavedWeeklyTimeSlots
    , requestTimeSlotPositions
    , requestTimeSlotsElement
    )

import Browser.Dom as Dom
import Http
import MainMsg exposing (Msg(..))
import Task
import TimeSlots.Messaging as TSMessaging
import TimeSlots.TimeSlots as TS


requestTimeSlotPositions : Cmd Msg
requestTimeSlotPositions =
    let
        getTimeSlotPosition slotNum =
            Dom.getElement (TS.getTimeSlotId 1 slotNum)
    in
    Task.attempt SetTimeSlotPositions (Task.sequence (List.map getTimeSlotPosition TS.slotNumRange))


requestTimeSlotsElement : Cmd Msg
requestTimeSlotsElement =
    Task.attempt SetTimeSlotsElement (Dom.getElement TS.scrollableTimeSlotsId)


requestSavedWeeklyTimeSlots : String -> Cmd Msg
requestSavedWeeklyTimeSlots userId =
    Http.get
        { url = "http://localhost:3000/api/" ++ userId ++ "/free-times"
        , expect = Http.expectJson SetSavedWeeklyTimeSlots TSMessaging.serverTimeSlotListDecoder
        }
