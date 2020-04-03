module TimeSlots.Update exposing (WithTimeSlotsEverything, update)

import Browser.Dom as Dom
import EventCreation.EventCreation as EC
import Flip
import Task
import TimeSlots.TimeSlots as TS
import Utils exposing (defaultOnError, defaultWithoutData, getListItemAt)


type alias WithTimeSlotsEverything a =
    EC.WithEventCreation (TS.WithTimeSlotPositions (TS.WithTimeSlotsElement (TS.WithTimeSlotSelection (TS.WithSelectedTimeSlots a))))


update : TS.Msg -> WithTimeSlotsEverything a -> ( WithTimeSlotsEverything a, Cmd TS.Msg )
update msg model =
    let
        noUpdateWithoutData =
            defaultWithoutData ( model, Cmd.none )

        noUpdateOnError =
            defaultOnError ( model, Cmd.none )

        useWithoutCmdMsg fn =
            Flip.flip Tuple.pair Cmd.none << fn

        noUpdateIfIntersectsSelectedTS dayNum startBound endBound updatedModel =
            if TS.intersectsCurrentlySelectedTimeSlots model.selectedTimeSlots dayNum startBound endBound then
                model

            else
                updatedModel
    in
    case msg of
        TS.SetTimeSlotPositions result ->
            let
                updateModelWithTSPositions elementList =
                    ( { model
                        | timeSlotPositions = TS.setTimeSlotPositions TS.startingSlotNum 0 elementList
                      }
                    , Cmd.none
                    )
            in
            noUpdateOnError result updateModelWithTSPositions

        TS.SetTimeSlotsElement result ->
            case result of
                Ok { element } ->
                    ( { model | timeSlotsElement = Just element }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        TS.StartSelectingTimeSlot dayNum slotNum ->
            let
                timeSlotPosition =
                    getListItemAt slotNum model.timeSlotPositions
            in
            noUpdateWithoutData
                timeSlotPosition
            <|
                useWithoutCmdMsg <|
                    noUpdateIfIntersectsSelectedTS dayNum slotNum slotNum
                        << TS.useTSPositionForBothSelectionBounds model dayNum

        TS.HandleTimeSlotMouseMove pointerPosition ->
            ( model, Task.attempt (TS.AdjustTimeSlotSelection pointerPosition) (Dom.getViewportOf TS.scrollableTimeSlotsId) )

        TS.AdjustTimeSlotSelection { pageY } result ->
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
                    noUpdateWithoutData
                        maybePointerTSPosition
                    <|
                        useWithoutCmdMsg updateSelectionWithPointerPosition

                ( _, _, _ ) ->
                    ( model, Cmd.none )

        TS.SetSelectedTimeSlot ->
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
