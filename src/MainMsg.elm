module MainMsg exposing (Msg(..), WithMdc)

import Browser.Dom as Dom
import EventCreation.EventCreation as EC
import Material
import TimeSlots.TimeSlots as TS


type Msg
    = NoOp
    | SetTimeSlotPositions (Result Dom.Error (List Dom.Element))
    | SetTimeSlotsElement (Result Dom.Error Dom.Element)
    | StartSelectingTimeSlot TS.DayNum TS.SlotNum
    | HandleTimeSlotMouseMove TS.PointerPosition
    | AdjustTimeSlotSelection TS.PointerPosition (Result Dom.Error Dom.Viewport)
    | SetSelectedTimeSlot
    | EventCreationMsg EC.Msg
    | Mdc (Material.Msg Msg)


type alias WithMdc a =
    { a | mdc : Material.Model Msg }
