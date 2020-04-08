module TimeSlots.Commands exposing (requestTimeSlotPositions, requestTimeSlotsElement)

import Browser.Dom as Dom
import MainMsg exposing (Msg(..))
import Task
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
