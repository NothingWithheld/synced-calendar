module TimeSlots exposing (..)


type alias DayNum =
    Int


type alias SlotNum =
    Int


type alias TimeSlotBoundaryPosition =
    { slotNum : SlotNum
    , y : Float
    , height : Float
    }


type alias SelectedTimeSlot =
    { dayNum : DayNum
    , startBound : TimeSlotBoundaryPosition
    , endBound : TimeSlotBoundaryPosition
    }


type alias WithSelectedTimeSlot a =
    { a
        | dayNum : DayNum
        , startBound : TimeSlotBoundaryPosition
        , endBound : TimeSlotBoundaryPosition
    }


type TimeSlotSelection
    = NotSelecting
    | CurrentlySelecting SelectedTimeSlot


startingDayNum : DayNum
startingDayNum =
    0


startingSlotNum : SlotNum
startingSlotNum =
    0


type alias WithTimeSlotSelection a =
    { a | timeSlotSelection : TimeSlotSelection }


setTimeSlotSelectionBounds :
    WithTimeSlotSelection a
    -> SelectedTimeSlot
    -> WithTimeSlotSelection a
setTimeSlotSelectionBounds beginningSelectionRecord timeSlotSelection =
    { beginningSelectionRecord
        | timeSlotSelection =
            CurrentlySelecting timeSlotSelection
    }


useTSPositionForBothSelectionBounds :
    WithTimeSlotSelection a
    -> DayNum
    -> TimeSlotBoundaryPosition
    -> WithTimeSlotSelection a
useTSPositionForBothSelectionBounds beginningSelectionRecord dayNum timeSlotPosition =
    setTimeSlotSelectionBounds beginningSelectionRecord <|
        SelectedTimeSlot dayNum timeSlotPosition timeSlotPosition


useTSPositionForEndSelectionBound :
    WithTimeSlotSelection a
    -> DayNum
    -> TimeSlotBoundaryPosition
    -> TimeSlotBoundaryPosition
    -> WithTimeSlotSelection a
useTSPositionForEndSelectionBound beginningSelectionRecord dayNum startBound timeSlotPosition =
    setTimeSlotSelectionBounds beginningSelectionRecord <|
        SelectedTimeSlot dayNum startBound timeSlotPosition
