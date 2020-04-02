module TimeSlots exposing (..)

import Browser.Dom as Dom
import Flip
import Task
import Utils exposing (defaultOnError, defaultWithoutData, getListItemAt)


scrollableTimeSlotsId : String
scrollableTimeSlotsId =
    "scrollable-time-slots"


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


type alias WithSelectedTimeSlot a =
    { a
        | dayNum : DayNum
        , startBound : TimeSlotBoundaryPosition
        , endBound : TimeSlotBoundaryPosition
    }


type TimeSlotSelection
    = NotSelecting
    | CurrentlySelecting SelectedTimeSlot


type alias WithTimeSlotSelection a =
    { a | timeSlotSelection : TimeSlotSelection }


type alias WithSelectedTimeSlots a b =
    { b | selectedTimeSlots : List (WithSelectedTimeSlot a) }


startingDayNum : DayNum
startingDayNum =
    0


startingSlotNum : SlotNum
startingSlotNum =
    0


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


intersectsCurrentlySelectedTimeSlots : List (WithSelectedTimeSlot a) -> DayNum -> Int -> Int -> Bool
intersectsCurrentlySelectedTimeSlots currentTimeSlots dayNum startSlotNum endSlotNum =
    let
        ( lowerSlotNum, higherSlotNum ) =
            if startSlotNum < endSlotNum then
                ( startSlotNum, endSlotNum )

            else
                ( endSlotNum, startSlotNum )

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


type alias PointerPosition =
    { pageX : Float
    , pageY : Float
    }


type Msg
    = SetTimeSlotPositions (Result Dom.Error (List Dom.Element))
    | SetTimeSlotsElement (Result Dom.Error Dom.Element)
    | StartSelectingTimeSlot Int Int
    | HandleTimeSlotMouseMove PointerPosition
    | AdjustTimeSlotSelection PointerPosition (Result Dom.Error Dom.Viewport)
    | SetSelectedTimeSlot


type alias WithTimeSlotsEverything a b =
    WithTimeSlotPositions (WithTimeSlotsElement (WithTimeSlotSelection (WithSelectedTimeSlots a b)))


update : Msg -> WithTimeSlotsEverything a b -> ( WithTimeSlotsEverything a b, Cmd Msg )
update msg model =
    let
        noUpdateWithoutData =
            defaultWithoutData ( model, Cmd.none )

        noUpdateOnError =
            defaultOnError ( model, Cmd.none )

        useWithoutCmdMsg fn =
            Flip.flip Tuple.pair Cmd.none << fn

        noUpdateIfIntersectsSelectedTS dayNum startBound endBound updatedModel =
            if intersectsCurrentlySelectedTimeSlots model.selectedTimeSlots dayNum startBound endBound then
                model

            else
                updatedModel
    in
    case msg of
        SetTimeSlotPositions result ->
            let
                updateModelWithTSPositions elementList =
                    ( { model
                        | timeSlotPositions = setTimeSlotPositions startingSlotNum 0 elementList
                      }
                    , Cmd.none
                    )
            in
            noUpdateOnError result updateModelWithTSPositions

        SetTimeSlotsElement result ->
            case result of
                Ok { element } ->
                    ( { model | timeSlotsElement = Just element }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        StartSelectingTimeSlot dayNum slotNum ->
            let
                timeSlotPosition =
                    getListItemAt slotNum model.timeSlotPositions
            in
            noUpdateWithoutData
                timeSlotPosition
            <|
                useWithoutCmdMsg <|
                    noUpdateIfIntersectsSelectedTS dayNum slotNum slotNum
                        << useTSPositionForBothSelectionBounds model dayNum

        HandleTimeSlotMouseMove pointerPosition ->
            ( model, Task.attempt (AdjustTimeSlotSelection pointerPosition) (Dom.getViewportOf scrollableTimeSlotsId) )

        AdjustTimeSlotSelection { pageY } result ->
            case ( result, model.timeSlotSelection, model.timeSlotsElement ) of
                ( Ok { viewport }, CurrentlySelecting { dayNum, startBound }, Just { y } ) ->
                    let
                        yPositionInTimeSlots =
                            pageY - y + viewport.y

                        maybePointerTSPosition =
                            getTimeSlotPositionOfPointer model.timeSlotPositions yPositionInTimeSlots

                        updateSelectionWithPointerPosition pointerTSPosition =
                            noUpdateIfIntersectsSelectedTS
                                dayNum
                                startBound.slotNum
                                pointerTSPosition.slotNum
                            <|
                                useTSPositionForEndSelectionBound
                                    model
                                    dayNum
                                    startBound
                                    pointerTSPosition
                    in
                    noUpdateWithoutData
                        maybePointerTSPosition
                    <|
                        useWithoutCmdMsg updateSelectionWithPointerPosition

                ( _, _, _ ) ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )
