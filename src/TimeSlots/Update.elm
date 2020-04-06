module TimeSlots.Update exposing
    ( adjustTimeSlotSelection
    , handleTimeSlotMouseMove
    , handleTimeSlotMouseUp
    , setSelectedTimeSlot
    , setTimeSlotPositions
    , setTimeSlotsElement
    , startSelectingTimeSlot
    )

import Browser.Dom as Dom
import EventCreation.EventCreation as EC
import EventCreation.Update as ECUpdate
import MainMsg exposing (Msg(..))
import Task
import TimeSlots.TimeSlots as TS
import Utils exposing (defaultOnError, defaultWithoutData, getListItemAt, useWithoutCmdMsg)


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
                << TS.useTSPositionForInitialSelection model dayNum


handleTimeSlotMouseMove : TS.WithTimeSlotSelection a -> TS.PointerPosition -> ( TS.WithTimeSlotSelection a, Cmd Msg )
handleTimeSlotMouseMove model pointerPosition =
    let
        adjustTSSelectionMsg =
            Task.attempt (AdjustTimeSlotSelection pointerPosition) (Dom.getViewportOf TS.scrollableTimeSlotsId)
    in
    case model.timeSlotSelection of
        TS.InitialPressNoMove { dayNum, startBound } ->
            ( TS.useTSPositionForBothSelectionBounds model dayNum startBound, adjustTSSelectionMsg )

        _ ->
            ( model, adjustTSSelectionMsg )


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


setOneHourSelection :
    TS.WithTimeSlotSelection (TS.WithTimeSlotPositions a)
    -> TS.DayNum
    -> TS.SlotNum
    -> ( TS.WithTimeSlotSelection (TS.WithTimeSlotPositions a), Cmd Msg )
setOneHourSelection model dayNum slotNum =
    let
        halfHourAdjustedSlotNum =
            2 * (slotNum // 2)

        endSlotNum =
            min (halfHourAdjustedSlotNum + 4) (TS.defaultNumSlots - 1)

        startBound =
            getListItemAt halfHourAdjustedSlotNum model.timeSlotPositions

        endBound =
            getListItemAt endSlotNum model.timeSlotPositions
    in
    case ( startBound, endBound ) of
        ( Just startBoundData, Just endBoundData ) ->
            ECUpdate.initiateUserPromptForEventDetails
                { model
                    | timeSlotSelection =
                        TS.CurrentlySelecting
                            { dayNum = dayNum
                            , startBound = startBoundData
                            , endBound = endBoundData
                            }
                }

        ( _, _ ) ->
            ( model, Cmd.none )


handleTimeSlotMouseUp :
    TS.WithTimeSlotSelection (TS.WithTimeSlotPositions a)
    -> ( TS.WithTimeSlotSelection (TS.WithTimeSlotPositions a), Cmd Msg )
handleTimeSlotMouseUp model =
    case model.timeSlotSelection of
        TS.CurrentlySelecting { dayNum, startBound, endBound } ->
            let
                minSlotNum =
                    min startBound.slotNum endBound.slotNum
            in
            ( model
            , Task.attempt
                PromptUserForEventDetails
                (Dom.getElement (TS.getTimeSlotId dayNum minSlotNum))
            )

        TS.InitialPressNoMove { dayNum, startBound } ->
            setOneHourSelection model dayNum startBound.slotNum

        _ ->
            ( model, Cmd.none )
