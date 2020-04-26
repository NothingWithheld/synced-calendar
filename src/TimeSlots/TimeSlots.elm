module TimeSlots.TimeSlots exposing (..)

import Browser.Dom as Dom
import EventCreation.EventCreation as EC
import Utils exposing (applyTwice, getMinMax)


scrollableTimeSlotsId : String
scrollableTimeSlotsId =
    "scrollable-time-slots"


type alias WithLoadingTimeSlots a =
    { a
        | loadingTimeSlots : Bool
    }


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


type alias SelectingTimeSlot =
    { dayNum : DayNum
    , startBound : TimeSlotBoundaryPosition
    , endBound : TimeSlotBoundaryPosition
    }


type alias WithSelectingTimeSlot a =
    { a
        | dayNum : DayNum
        , startBound : TimeSlotBoundaryPosition
        , endBound : TimeSlotBoundaryPosition
    }


type alias SelectedTimeSlot =
    { dayNum : DayNum
    , id : Int
    , startBound : TimeSlotBoundaryPosition
    , endBound : TimeSlotBoundaryPosition
    }


type alias WithSelectedTimeSlot a =
    { a
        | dayNum : DayNum
        , id : Int
        , startBound : TimeSlotBoundaryPosition
        , endBound : TimeSlotBoundaryPosition
    }


type alias TimeSlotInitialSelection =
    { dayNum : DayNum
    , startBound : TimeSlotBoundaryPosition
    }


type alias WithTimeSlotInitialSelection a =
    { a
        | dayNum : DayNum
        , startBound : TimeSlotBoundaryPosition
    }


type SelectedTimeSlotDetails
    = SelectedTimeSlotDetails SelectedTimeSlot EC.EventCreationDetails


type TimeSlotSelection
    = NotSelecting
    | InitialPressNoMove TimeSlotInitialSelection
    | CurrentlySelecting SelectingTimeSlot
    | EditingSelection SelectingTimeSlot SelectedTimeSlotDetails


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


defaultNumSlots : Int
defaultNumSlots =
    24 * 4


defaultNumDays : Int
defaultNumDays =
    7


maxDayNum : DayNum
maxDayNum =
    defaultNumDays - 1


maxSlotNum : SlotNum
maxSlotNum =
    defaultNumSlots - 1


dayNumRange : List DayNum
dayNumRange =
    List.range startingDayNum maxDayNum


slotNumRange : List SlotNum
slotNumRange =
    List.range startingSlotNum maxSlotNum


dayAbbreviations : List String
dayAbbreviations =
    [ "SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT" ]


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
    -> SelectingTimeSlot
    -> WithTimeSlotSelection a
setTimeSlotSelectionBounds beginningSelectionRecord selectionBounds =
    case beginningSelectionRecord.timeSlotSelection of
        EditingSelection _ prevDetails ->
            { beginningSelectionRecord
                | timeSlotSelection =
                    EditingSelection selectionBounds prevDetails
            }

        _ ->
            { beginningSelectionRecord
                | timeSlotSelection =
                    CurrentlySelecting selectionBounds
            }


useTSPositionForBothSelectionBounds :
    WithTimeSlotSelection a
    -> DayNum
    -> TimeSlotBoundaryPosition
    -> WithTimeSlotSelection a
useTSPositionForBothSelectionBounds beginningSelectionRecord dayNum timeSlotPosition =
    setTimeSlotSelectionBounds beginningSelectionRecord <|
        SelectingTimeSlot dayNum timeSlotPosition timeSlotPosition


useTSPositionsForSelectionBounds :
    WithTimeSlotSelection a
    -> DayNum
    -> TimeSlotBoundaryPosition
    -> TimeSlotBoundaryPosition
    -> WithTimeSlotSelection a
useTSPositionsForSelectionBounds beginningSelectionRecord dayNum startBound timeSlotPosition =
    setTimeSlotSelectionBounds beginningSelectionRecord <|
        SelectingTimeSlot dayNum startBound timeSlotPosition


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


doesTSSelectionIntersectSelectedTimeSlots : List SelectedTimeSlotDetails -> TimeSlotSelection -> Bool
doesTSSelectionIntersectSelectedTimeSlots currentTimeSlotDetails timeSlotSelection =
    case timeSlotSelection of
        CurrentlySelecting { dayNum, startBound, endBound } ->
            intersectsCurrentlySelectedTimeSlots currentTimeSlotDetails
                dayNum
                startBound.slotNum
                endBound.slotNum

        EditingSelection { dayNum, startBound, endBound } _ ->
            intersectsCurrentlySelectedTimeSlots currentTimeSlotDetails
                dayNum
                startBound.slotNum
                endBound.slotNum

        _ ->
            False


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


getOrderedTimeSlot : WithSelectingTimeSlot a -> WithSelectingTimeSlot a
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


getTimeForSlotNum : SlotNum -> Bool -> ( String, String )
getTimeForSlotNum slotNum isEndSlot =
    let
        adjustedSlotNum =
            slotNum
                + (if isEndSlot then
                    1

                   else
                    0
                  )

        hour =
            modBy 12 <| adjustedSlotNum // 4

        adjustedHour =
            if hour == 0 then
                12

            else
                hour

        quarterInc =
            modBy 4 <| adjustedSlotNum

        amOrPm =
            if adjustedSlotNum < 12 * 4 || slotNum == maxSlotNum then
                "am"

            else
                "pm"
    in
    ( String.fromInt adjustedHour
        ++ (if quarterInc == 0 then
                ""

            else
                ":" ++ String.fromInt (15 * quarterInc)
           )
    , amOrPm
    )


getTimeDurationBetween : SlotNum -> SlotNum -> String
getTimeDurationBetween startSlot endSlot =
    let
        diff =
            endSlot + 1 - startSlot
    in
    if diff < 4 then
        (String.fromInt <| diff * 15) ++ " mins"

    else if diff == 4 then
        "1 hr"

    else
        (String.fromFloat <| toFloat diff / 4) ++ " hrs"


areSelectionBoundsEqual : WithSelectingTimeSlot a -> WithSelectingTimeSlot b -> Bool
areSelectionBoundsEqual selectionBoundsA selectionBoundsB =
    selectionBoundsA.dayNum
        == selectionBoundsB.dayNum
        && selectionBoundsA.startBound
        == selectionBoundsB.startBound
        && selectionBoundsA.endBound
        == selectionBoundsB.endBound


selectingToSelectedTimeSlot : SelectingTimeSlot -> Int -> SelectedTimeSlot
selectingToSelectedTimeSlot { dayNum, startBound, endBound } timeSlotId =
    SelectedTimeSlot dayNum
        timeSlotId
        startBound
        endBound


selectedToSelectingTimeSlot : SelectedTimeSlot -> SelectingTimeSlot
selectedToSelectingTimeSlot { dayNum, startBound, endBound } =
    SelectingTimeSlot dayNum startBound endBound
