module MainMsg exposing (Msg(..))

import Browser.Dom as Dom
import Material
import TimeSlots as TS


type Msg
    = NoOp
    | TimeSlotMsg TS.Msg
    | InitiateUserPromptForEventDetails
    | PromptUserForEventDetails (Result Dom.Error Dom.Element)
    | AdjustEventTitle String
    | AdjustEventDescription String
    | CloseUserPromptForEventDetails
    | SetSelectedTimeSlot
    | Mdc (Material.Msg Msg)
