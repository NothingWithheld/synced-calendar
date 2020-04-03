module MainMsg exposing (Msg(..), WithMdc)

import EventCreation.EventCreation as EC
import Material
import TimeSlots.TimeSlots as TS


type Msg
    = NoOp
    | TimeSlotMsg TS.Msg
    | EventCreationMsg EC.Msg
    | Mdc (Material.Msg Msg)


type alias WithMdc a =
    { a | mdc : Material.Model Msg }
