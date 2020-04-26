module WeeklyFreeTimes.MainMsg exposing (Msg(..), WithMdc)

import Browser.Dom as Dom
import Http
import Material
import TimeSlots.Messaging as TSMessaging
import TimeSlots.TimeSlots as TS
import Utils exposing (NoData)


type Msg
    = NoOp
      -- TimeSlots
    | SetTimeSlotPositions (Result Dom.Error (List Dom.Element))
    | UpdateTimeZone String
    | SetTimeSlotsElement (Result Dom.Error Dom.Element)
    | SetSavedWeeklyTimeSlots (Result Http.Error (List TSMessaging.ServerTimeSlot))
    | StartSelectingTimeSlot TS.DayNum TS.SlotNum
    | HandleTimeSlotMouseMove TS.PointerPosition
    | AdjustTimeSlotSelection TS.PointerPosition (Result Dom.Error Dom.Viewport)
    | SendSaveTimeSlotRequest
    | SendUpdateTimeSlotRequest
    | SetSelectedTimeSlotAfterCreation (Result Http.Error Int)
    | SetSelectedTimeSlotAfterEditing (Result Http.Error NoData)
    | HandleTimeSlotMouseUp
    | EditTimeSlotSelection TS.SelectedTimeSlotDetails
    | SendDeleteTimeSlotRequest
    | DeleteTimeSlot (Result Http.Error NoData)
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
    | SaveEditingTimeSlotWithoutChanges
    | Mdc (Material.Msg Msg)


type alias WithMdc a =
    { a | mdc : Material.Model Msg }
