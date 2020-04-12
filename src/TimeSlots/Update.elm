module TimeSlots.Update exposing
    ( adjustTimeSlotSelection
    , deleteTimeSlot
    , editTimeSlotSelection
    , handleTimeSlotMouseMove
    , handleTimeSlotMouseUp
    , sendDeleteTimeSlotRequest
    , sendSaveTimeSlotRequest
    , sendUpdateTimeSlotRequest
    , setSavedWeeklyTimeSlots
    , setSelectedTimeSlotAfterCreation
    , setSelectedTimeSlotAfterEditing
    , setTimeSlotPositions
    , setTimeSlotsElement
    , startSelectingTimeSlot
    )

import Browser.Dom as Dom
import EventCreation.EventCreation as EC
import EventCreation.Update as ECUpdate
import Flip
import Http
import MainMsg exposing (Msg(..))
import Task
import TimeSlots.Commands
    exposing
        ( deleteWeeklyTimeSlot
        , requestSavedWeeklyTimeSlots
        , saveWeeklyTimeSlot
        , updateWeeklyTimeSlot
        )
import TimeSlots.Messaging as TSMessaging
import TimeSlots.TimeSlots as TS
import Utils
    exposing
        ( NoData
        , applyTwice
        , defaultOnError
        , defaultWithoutData
        , getListItemAt
        , useWithoutCmdMsg
        )


setTimeSlotPositions :
    TS.WithTimeSlotPositions (TS.WithUserId a)
    -> Result Dom.Error (List Dom.Element)
    -> ( TS.WithTimeSlotPositions (TS.WithUserId a), Cmd Msg )
setTimeSlotPositions model result =
    let
        updateModelWithTSPositions elementList =
            ( { model
                | timeSlotPositions = TS.setTimeSlotPositions TS.startingSlotNum 0 elementList
              }
            , requestSavedWeeklyTimeSlots model.userId
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


setSavedWeeklyTimeSlots :
    TS.WithLoadingTimeSlots (TS.WithTimeSlotPositions (TS.WithSelectedTimeSlots a))
    -> Result Http.Error (List TSMessaging.ServerTimeSlot)
    -> ( TS.WithLoadingTimeSlots (TS.WithTimeSlotPositions (TS.WithSelectedTimeSlots a)), Cmd Msg )
setSavedWeeklyTimeSlots model result =
    let
        updateWithTimeSlot { dayNum, startSlot, endSlot, id } model_ =
            let
                startBound =
                    getListItemAt startSlot model_.timeSlotPositions

                endBound =
                    getListItemAt endSlot model_.timeSlotPositions
            in
            case Maybe.map2 (TS.SelectedTimeSlot dayNum id) startBound endBound of
                Just selectionBounds ->
                    { model_
                        | selectedTimeSlots =
                            TS.SelectedTimeSlotDetails selectionBounds EC.WeeklyFreeTimes
                                :: model_.selectedTimeSlots
                    }

                Nothing ->
                    model_
    in
    case result of
        Ok timeSlotList ->
            ( List.foldl
                updateWithTimeSlot
                { model | loadingTimeSlots = False }
                timeSlotList
            , Cmd.none
            )

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
                        TS.useTSPositionsForSelectionBounds
                            model
                            dayNum
                            startBound
                            pointerTSPosition
            in
            defaultWithoutData ( model, Cmd.none ) maybePointerTSPosition <|
                useWithoutCmdMsg updateSelectionWithPointerPosition

        ( _, _, _ ) ->
            ( model, Cmd.none )


sendSaveTimeSlotRequest : TS.WithTimeSlotSelection (TS.WithUserId a) -> ( TS.WithTimeSlotSelection (TS.WithUserId a), Cmd Msg )
sendSaveTimeSlotRequest model =
    case model.timeSlotSelection of
        TS.CurrentlySelecting selectionBounds ->
            let
                { dayNum, startBound, endBound } =
                    TS.getOrderedTimeSlot selectionBounds
            in
            ( model
            , saveWeeklyTimeSlot model.userId
                dayNum
                startBound.slotNum
                endBound.slotNum
            )

        _ ->
            ( model, Cmd.none )


sendUpdateTimeSlotRequest : TS.WithTimeSlotSelection a -> ( TS.WithTimeSlotSelection a, Cmd Msg )
sendUpdateTimeSlotRequest model =
    case model.timeSlotSelection of
        TS.EditingSelection selectionBounds prevSelection ->
            let
                { dayNum, startBound, endBound } =
                    TS.getOrderedTimeSlot selectionBounds

                timeSlotId =
                    .id <| TS.getTimeSlotFromDetails prevSelection
            in
            ( model
            , updateWeeklyTimeSlot timeSlotId
                dayNum
                startBound.slotNum
                endBound.slotNum
            )

        _ ->
            ( model, Cmd.none )


setSelectedTimeSlotAfterCreation :
    EC.WithEventCreation (TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection a))
    -> Result Http.Error Int
    -> ( EC.WithEventCreation (TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection a)), Cmd Msg )
setSelectedTimeSlotAfterCreation model result =
    case ( model.eventCreation, model.timeSlotSelection, result ) of
        ( EC.CurrentlyCreatingEvent eventDetails _, TS.CurrentlySelecting selectionBounds, Ok timeSlotId ) ->
            let
                orderedSelectionBounds =
                    TS.getOrderedTimeSlot selectionBounds

                selectedTimeSlot =
                    TS.SelectedTimeSlotDetails
                        (TS.selectingToSelectedTimeSlot orderedSelectionBounds timeSlotId)
                        eventDetails

                intersectsTimeSlots =
                    TS.doesTSSelectionIntersectSelectedTimeSlots
                        model.selectedTimeSlots
                        model.timeSlotSelection
            in
            if intersectsTimeSlots then
                ( model, Cmd.none )

            else
                ( { model
                    | selectedTimeSlots = selectedTimeSlot :: model.selectedTimeSlots
                    , timeSlotSelection = TS.NotSelecting
                    , eventCreation = EC.NotCreating
                  }
                , Cmd.none
                )

        _ ->
            ( model, Cmd.none )


setSelectedTimeSlotAfterEditing :
    EC.WithEventCreation (TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection a))
    -> Result Http.Error NoData
    -> ( EC.WithEventCreation (TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection a)), Cmd Msg )
setSelectedTimeSlotAfterEditing model result =
    case ( model.eventCreation, model.timeSlotSelection, result ) of
        ( EC.CurrentlyCreatingEvent eventDetails _, TS.EditingSelection selectionBounds prevSelection, Ok _ ) ->
            let
                orderedSelectionBounds =
                    TS.getOrderedTimeSlot selectionBounds

                selectedTimeSlot =
                    TS.SelectedTimeSlotDetails
                        (TS.selectingToSelectedTimeSlot orderedSelectionBounds <|
                            .id <|
                                TS.getTimeSlotFromDetails prevSelection
                        )
                        eventDetails

                intersectsTimeSlots =
                    TS.doesTSSelectionIntersectSelectedTimeSlots
                        model.selectedTimeSlots
                        model.timeSlotSelection
            in
            if intersectsTimeSlots then
                ( model, Cmd.none )

            else
                ( { model
                    | selectedTimeSlots = selectedTimeSlot :: model.selectedTimeSlots
                    , timeSlotSelection = TS.NotSelecting
                    , eventCreation = EC.NotCreating
                  }
                , Cmd.none
                )

        _ ->
            ( model, Cmd.none )


setOneHourSelection :
    TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection (TS.WithTimeSlotPositions a))
    -> TS.DayNum
    -> TS.SlotNum
    -> ( TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection (TS.WithTimeSlotPositions a)), Cmd Msg )
setOneHourSelection model dayNum slotNum =
    let
        halfHourAdjustedSlotNum =
            2 * (slotNum // 2)

        endSlotNum =
            min (halfHourAdjustedSlotNum + 3) (TS.defaultNumSlots - 1)

        unselectedTSRange =
            TS.getUnselectedTimeSlotRange
                model.selectedTimeSlots
                dayNum
                halfHourAdjustedSlotNum
                endSlotNum

        maybeBounds =
            Maybe.map
                (applyTwice
                    Tuple.mapBoth
                 <|
                    Flip.flip getListItemAt <|
                        model.timeSlotPositions
                )
                unselectedTSRange
    in
    case maybeBounds of
        Just ( maybeStart, maybeEnd ) ->
            case ( maybeStart, maybeEnd ) of
                ( Just startBound, Just endBound ) ->
                    ECUpdate.initiateUserPromptForEventDetails
                        { model
                            | timeSlotSelection =
                                TS.CurrentlySelecting
                                    { dayNum = dayNum
                                    , startBound = startBound
                                    , endBound = endBound
                                    }
                        }

                ( _, _ ) ->
                    ( model, Cmd.none )

        Nothing ->
            ( { model | timeSlotSelection = TS.NotSelecting }, Cmd.none )


handleTimeSlotMouseUp :
    TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection (TS.WithTimeSlotPositions a))
    -> ( TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection (TS.WithTimeSlotPositions a)), Cmd Msg )
handleTimeSlotMouseUp model =
    case model.timeSlotSelection of
        TS.InitialPressNoMove { dayNum, startBound } ->
            setOneHourSelection model dayNum startBound.slotNum

        TS.CurrentlySelecting _ ->
            ECUpdate.initiateUserPromptForEventDetails model

        _ ->
            ( model, Cmd.none )


editTimeSlotSelection :
    TS.WithTimeSlotSelection (TS.WithSelectedTimeSlots a)
    -> TS.SelectedTimeSlotDetails
    -> ( TS.WithTimeSlotSelection (TS.WithSelectedTimeSlots a), Cmd Msg )
editTimeSlotSelection model selectedTimeSlotDetails =
    let
        (TS.SelectedTimeSlotDetails selectedTimeSlot _) =
            selectedTimeSlotDetails

        selectedTimeSlotsWithoutChosen =
            List.filter ((/=) selectedTimeSlotDetails) model.selectedTimeSlots
    in
    ECUpdate.initiateUserPromptForEventDetails
        { model
            | timeSlotSelection =
                TS.EditingSelection (TS.selectedToSelectingTimeSlot selectedTimeSlot) selectedTimeSlotDetails
            , selectedTimeSlots = selectedTimeSlotsWithoutChosen
        }


sendDeleteTimeSlotRequest : TS.WithTimeSlotSelection a -> ( TS.WithTimeSlotSelection a, Cmd Msg )
sendDeleteTimeSlotRequest model =
    case model.timeSlotSelection of
        TS.EditingSelection _ prevSelection ->
            let
                timeSlotId =
                    .id <| TS.getTimeSlotFromDetails prevSelection
            in
            ( model
            , deleteWeeklyTimeSlot timeSlotId
            )

        _ ->
            ( model, Cmd.none )


deleteTimeSlot :
    EC.WithDiscardConfirmationModal (EC.WithEventCreation (TS.WithTimeSlotSelection a))
    -> Result Http.Error NoData
    -> ( EC.WithDiscardConfirmationModal (EC.WithEventCreation (TS.WithTimeSlotSelection a)), Cmd Msg )
deleteTimeSlot model result =
    case result of
        Ok _ ->
            ECUpdate.closeUserPromptForEventDetails model

        _ ->
            ( model, Cmd.none )
