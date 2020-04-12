module MainMsg exposing (Msg(..), WithMdc)

import Browser.Dom as Dom
import Http
import Material
import TimeSlots.Messaging as TSMessaging
import TimeSlots.TimeSlots as TS


type Msg
    = NoOp
      -- TimeSlots
    | SetTimeSlotPositions (Result Dom.Error (List Dom.Element))
    | SetTimeSlotsElement (Result Dom.Error Dom.Element)
    | SetSavedWeeklyTimeSlots (Result Http.Error (List TSMessaging.ServerTimeSlot))
    | StartSelectingTimeSlot TS.DayNum TS.SlotNum
    | HandleTimeSlotMouseMove TS.PointerPosition
    | AdjustTimeSlotSelection TS.PointerPosition (Result Dom.Error Dom.Viewport)
    | SetSelectedTimeSlot
    | HandleTimeSlotMouseUp
    | EditTimeSlotSelection TS.SelectedTimeSlotDetails
      -- EventCreation
    | PromptUserForEventDetails (Result Dom.Error Dom.Element)
    | AdjustEventTitle String
    | AdjustEventDescription String
    | ChangeSelectionDayNum String
    | ChangeSelectionStartSlot String
    | ChangeSelectionEndSlot String
    | HandleEditingCancel
    | CloseUserPromptForEventDetails
    | CancelDiscardConfirmationModal
    | Mdc (Material.Msg Msg)


type alias WithMdc a =
    { a | mdc : Material.Model Msg }
