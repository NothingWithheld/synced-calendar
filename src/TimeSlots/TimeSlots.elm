module TimeSlots.TimeSlots exposing (..)

import Browser.Dom as Dom
import EventCreation.EventCreation as EC
import Utils exposing (applyTwice, getMinMax)


scrollableTimeSlotsId : String
scrollableTimeSlotsId =
    "scrollable-time-slots"


defaultNumSlots : Int
defaultNumSlots =
    24 * 4


defaultNumDays : Int
defaultNumDays =
    7


type alias DayNum =
    Int


type alias SlotNum =
    Int


type alias TimeSlotBoundaryPosition =
    { slotNum : SlotNum
    , y : Float
    , height : Float
    }


type alias WithTimeSlotPositions a =
    { a | timeSlotPositions : List TimeSlotBoundaryPosition }


type alias Element =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias WithTimeSlotsElement a =
    { a | timeSlotsElement : Maybe Element }


type alias SelectedTimeSlot =
    { dayNum : DayNum
    , startBound : TimeSlotBoundaryPosition
    , endBound : TimeSlotBoundaryPosition
    }


type alias InitialSelectedTimeSlot =
    { dayNum : DayNum
    , startBound : TimeSlotBoundaryPosition
    }


type alias WithInitialSelectedTimeSlot a =
    { a
        | dayNum : DayNum
        , startBound : TimeSlotBoundaryPosition
    }


type SelectedTimeSlotDetails
    = SelectedTimeSlotDetails SelectedTimeSlot EC.EventCreationDetails


type alias WithSelectedTimeSlot a =
    { a
        | dayNum : DayNum
        , startBound : TimeSlotBoundaryPosition
        , endBound : TimeSlotBoundaryPosition
    }


type TimeSlotSelection
    = NotSelecting
    | InitialPressNoMove InitialSelectedTimeSlot
    | CurrentlySelecting SelectedTimeSlot


type alias WithTimeSlotSelection a =
    { a | timeSlotSelection : TimeSlotSelection }


type alias WithSelectedTimeSlots a =
    { a | selectedTimeSlots : List SelectedTimeSlotDetails }


startingDayNum : DayNum
startingDayNum =
    0


startingSlotNum : SlotNum
startingSlotNum =
    0


useTSPositionForInitialSelection :
    WithTimeSlotSelection a
    -> DayNum
    -> TimeSlotBoundaryPosition
    -> WithTimeSlotSelection a
useTSPositionForInitialSelection beginningSelectionRecord dayNum timeSlotPosition =
    { beginningSelectionRecord
        | timeSlotSelection =
            InitialPressNoMove
                { dayNum = dayNum
                , startBound =
                    timeSlotPosition
                }
    }


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


setTimeSlotPositions : Int -> Float -> List Dom.Element -> List TimeSlotBoundaryPosition
setTimeSlotPositions ind curYOffset elements =
    case elements of
        [] ->
            []

        { element } :: xs ->
            { slotNum = ind
            , y = curYOffset
            , height = element.height
            }
                :: setTimeSlotPositions (ind + 1) (curYOffset + element.height) xs


intersectsCurrentlySelectedTimeSlots : List SelectedTimeSlotDetails -> DayNum -> SlotNum -> SlotNum -> Bool
intersectsCurrentlySelectedTimeSlots currentTimeSlotsDetails dayNum startSlotNum endSlotNum =
    let
        currentTimeSlots =
            List.map getTimeSlotFromDetails currentTimeSlotsDetails

        ( lowerSlotNum, higherSlotNum ) =
            getMinMax startSlotNum endSlotNum

        selectedTimeSlotsForThisDay =
            List.filter (\timeSlot -> timeSlot.dayNum == dayNum) currentTimeSlots

        isTimeSlotTaken timeSlot =
            List.any
                (\selected ->
                    timeSlot >= selected.startBound.slotNum && timeSlot <= selected.endBound.slotNum
                )
                selectedTimeSlotsForThisDay
    in
    List.any isTimeSlotTaken (List.range lowerSlotNum higherSlotNum)


getUnselectedTimeSlotRange : List SelectedTimeSlotDetails -> DayNum -> SlotNum -> SlotNum -> Maybe ( SlotNum, SlotNum )
getUnselectedTimeSlotRange currentTimeSlotDetails dayNum startSlotNum endSlotNum =
    let
        ( lowerSlotNum, higherSlotNum ) =
            getMinMax startSlotNum endSlotNum

        unselectedRange =
            List.filter
                (not
                    << (applyTwice <|
                            intersectsCurrentlySelectedTimeSlots
                                currentTimeSlotDetails
                                dayNum
                       )
                )
            <|
                List.range lowerSlotNum higherSlotNum

        rangeStart =
            List.head unselectedRange

        getContinuousRange start =
            Tuple.pair start <|
                List.foldl
                    (\( i, slotNum ) end ->
                        if start + i == slotNum then
                            slotNum

                        else
                            end
                    )
                    start
                <|
                    List.indexedMap Tuple.pair unselectedRange
    in
    if List.isEmpty unselectedRange then
        Nothing

    else
        Maybe.map getContinuousRange rangeStart


getTimeSlotFromDetails : SelectedTimeSlotDetails -> WithSelectedTimeSlot {}
getTimeSlotFromDetails (SelectedTimeSlotDetails timeSlot _) =
    timeSlot


getTimeSlotPositionOfPointer : List TimeSlotBoundaryPosition -> Float -> Maybe TimeSlotBoundaryPosition
getTimeSlotPositionOfPointer timeSlotPositions pageY =
    let
        getTSPOPHelper curTSPosList =
            case curTSPosList of
                [] ->
                    Nothing

                curTSPos :: rest ->
                    if (curTSPos.y <= pageY) && (pageY <= curTSPos.y + curTSPos.height) then
                        Just curTSPos

                    else
                        getTSPOPHelper rest
    in
    getTSPOPHelper timeSlotPositions


getOrderedTimeSlot : WithSelectedTimeSlot a -> WithSelectedTimeSlot a
getOrderedTimeSlot timeSlot =
    let
        { startBound, endBound } =
            timeSlot

        ( topSlot, bottomSlot ) =
            if startBound.slotNum <= endBound.slotNum then
                ( startBound, endBound )

            else
                ( endBound, startBound )
    in
    { timeSlot | startBound = topSlot, endBound = bottomSlot }


getTimeSlotId : DayNum -> SlotNum -> String
getTimeSlotId dayNum slotNum =
    "time-slot-" ++ String.fromInt dayNum ++ "--" ++ String.fromInt slotNum


type alias PointerPosition =
    { pageX : Float
    , pageY : Float
    }
