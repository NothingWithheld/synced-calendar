module TimeSlots.Update exposing
    ( WithTimeSlotsEverything
    , adjustTimeSlotSelection
    , handleTimeSlotMouseMove
    , setSelectedTimeSlot
    , setTimeSlotPositions
    , setTimeSlotsElement
    , startSelectingTimeSlot
    )

import Browser.Dom as Dom
import EventCreation.EventCreation as EC
import MainMsg exposing (Msg(..))
import Task
import TimeSlots.TimeSlots as TS
import Utils exposing (defaultOnError, defaultWithoutData, getListItemAt, useWithoutCmdMsg)


type alias WithTimeSlotsEverything a =
    EC.WithEventCreation (TS.WithTimeSlotPositions (TS.WithTimeSlotsElement (TS.WithTimeSlotSelection (TS.WithSelectedTimeSlots a))))


setTimeSlotPositions : TS.WithTimeSlotPositions a -> Result Dom.Error (List Dom.Element) -> ( TS.WithTimeSlotPositions a, Cmd Msg )
setTimeSlotPositions model result =
    let
        updateModelWithTSPositions elementList =
            ( { model
                | timeSlotPositions = TS.setTimeSlotPositions TS.startingSlotNum 0 elementList
              }
            , Cmd.none
            )
    in
    defaultOnError ( model, Cmd.none ) result updateModelWithTSPositions


setTimeSlotsElement : TS.WithTimeSlotsElement a -> Result Dom.Error Dom.Element -> ( TS.WithTimeSlotsElement a, Cmd Msg )
setTimeSlotsElement model result =
    case result of
        Ok { element } ->
            ( { model | timeSlotsElement = Just element }, Cmd.none )

        Err _ ->
            ( model, Cmd.none )


startSelectingTimeSlot :
    TS.WithSelectedTimeSlots (TS.WithTimeSlotPositions (TS.WithTimeSlotSelection a))
    -> TS.DayNum
    -> TS.SlotNum
    -> ( TS.WithSelectedTimeSlots (TS.WithTimeSlotPositions (TS.WithTimeSlotSelection a)), Cmd Msg )
startSelectingTimeSlot model dayNum slotNum =
    let
        noUpdateIfIntersectsSelectedTS dayNum_ startBound endBound updatedModel =
            if TS.intersectsCurrentlySelectedTimeSlots model.selectedTimeSlots dayNum_ startBound endBound then
                model

            else
                updatedModel

        timeSlotPosition =
            getListItemAt slotNum model.timeSlotPositions
    in
    defaultWithoutData ( model, Cmd.none ) timeSlotPosition <|
        useWithoutCmdMsg <|
            noUpdateIfIntersectsSelectedTS dayNum slotNum slotNum
                << TS.useTSPositionForBothSelectionBounds model dayNum


handleTimeSlotMouseMove : a -> TS.PointerPosition -> ( a, Cmd Msg )
handleTimeSlotMouseMove model pointerPosition =
    ( model, Task.attempt (AdjustTimeSlotSelection pointerPosition) (Dom.getViewportOf TS.scrollableTimeSlotsId) )


adjustTimeSlotSelection :
    TS.WithTimeSlotsElement (TS.WithSelectedTimeSlots (TS.WithTimeSlotPositions (TS.WithTimeSlotSelection a)))
    -> TS.PointerPosition
    -> Result Dom.Error Dom.Viewport
    -> ( TS.WithTimeSlotsElement (TS.WithSelectedTimeSlots (TS.WithTimeSlotPositions (TS.WithTimeSlotSelection a))), Cmd Msg )
adjustTimeSlotSelection model { pageY } result =
    let
        noUpdateIfIntersectsSelectedTS dayNum startBound endBound updatedModel =
            if TS.intersectsCurrentlySelectedTimeSlots model.selectedTimeSlots dayNum startBound endBound then
                model

            else
                updatedModel
    in
    case ( result, model.timeSlotSelection, model.timeSlotsElement ) of
        ( Ok { viewport }, TS.CurrentlySelecting { dayNum, startBound }, Just { y } ) ->
            let
                yPositionInTimeSlots =
                    pageY - y + viewport.y

                maybePointerTSPosition =
                    TS.getTimeSlotPositionOfPointer model.timeSlotPositions yPositionInTimeSlots

                updateSelectionWithPointerPosition pointerTSPosition =
                    noUpdateIfIntersectsSelectedTS
                        dayNum
                        startBound.slotNum
                        pointerTSPosition.slotNum
                    <|
                        TS.useTSPositionForEndSelectionBound
                            model
                            dayNum
                            startBound
                            pointerTSPosition
            in
            defaultWithoutData ( model, Cmd.none ) maybePointerTSPosition <|
                useWithoutCmdMsg updateSelectionWithPointerPosition

        ( _, _, _ ) ->
            ( model, Cmd.none )


setSelectedTimeSlot :
    EC.WithEventCreation (TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection a))
    -> ( EC.WithEventCreation (TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection a)), Cmd Msg )
setSelectedTimeSlot model =
    case ( model.eventCreation, model.timeSlotSelection ) of
        ( EC.CurrentlyCreatingEvent eventDetails _, TS.CurrentlySelecting timeSlot ) ->
            let
                orderedTimeSlot =
                    TS.getOrderedTimeSlot timeSlot

                selectedTimeSlot =
                    TS.SelectedTimeSlotDetails orderedTimeSlot eventDetails
            in
            ( { model
                | selectedTimeSlots = selectedTimeSlot :: model.selectedTimeSlots
                , timeSlotSelection = TS.NotSelecting
                , eventCreation = EC.NotCreating
              }
            , Cmd.none
            )

        ( _, _ ) ->
            ( model, Cmd.none )
