module TimeSlots.Update exposing
    ( adjustTimeSlotSelection
    , deleteTimeSlot
    , editTimeSlotSelection
    , handleTimeSlotMouseMove
    , handleTimeSlotMouseUp
    , sendDeleteTimeSlotRequest
    , sendSaveTimeSlotRequest
    , sendUpdateTimeSlotRequest
    , setInitialTime
    , setSavedWeeklyTimeSlots
    , setSelectedTimeSlotAfterCreation
    , setSelectedTimeSlotAfterEditing
    , setTimeSlotPositions
    , setTimeSlotsElement
    , startSelectingTimeSlot
    , updateTimeZone
    )

import Browser.Dom as Dom
import EventCreation.EventCreation as EC
import EventCreation.Update as ECUpdate
import Flip
import Http
import Session exposing (WithSession)
import Task
import Time exposing (Posix)
import TimeSlots.Commands
    exposing
        ( deleteWeeklyTimeSlot
        , requestSavedWeeklyTimeSlots
        , saveWeeklyTimeSlot
        , updateWeeklyTimeSlot
        )
import TimeSlots.Messaging as TSMessaging
import TimeSlots.Time as TSTime exposing (TimeDetails(..))
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


setInitialTime :
    TSTime.WithTimeDetails a
    -> Result Never Posix
    -> ( TSTime.WithTimeDetails a, Cmd msg )
setInitialTime model result =
    case result of
        Ok currentDay ->
            ( { model
                | timeDetails =
                    WithTime { currentDay = currentDay, weekOffset = 0 }
              }
            , Cmd.none
            )

        Err _ ->
            ( model, Cmd.none )


setTimeSlotPositions :
    TS.WithTimeSlotPositions (WithSession a)
    -> (Result Http.Error (List TSMessaging.ServerTimeSlot) -> msg)
    -> Result Dom.Error (List Dom.Element)
    -> ( TS.WithTimeSlotPositions (WithSession a), Cmd msg )
setTimeSlotPositions model setSavedWeeklyTS result =
    let
        updateModelWithTSPositions elementList =
            ( { model
                | timeSlotPositions = TS.setTimeSlotPositions TS.startingSlotNum 0 elementList
              }
            , requestSavedWeeklyTimeSlots
                setSavedWeeklyTS
                (Session.getUserId model.session)
                (Session.getOffset model.session)
            )
    in
    defaultOnError ( model, Cmd.none ) result updateModelWithTSPositions


updateTimeZone :
    TS.WithLoadingTimeSlots (WithSession a)
    -> (Result Http.Error (List TSMessaging.ServerTimeSlot) -> msg)
    -> String
    -> ( TS.WithLoadingTimeSlots (WithSession a), Cmd msg )
updateTimeZone model onGetUpdatedTimeSlots timeZoneLabel =
    case Session.setOffset model.session timeZoneLabel of
        Just newSession ->
            ( { model | session = newSession, loadingTimeSlots = True }
            , requestSavedWeeklyTimeSlots
                onGetUpdatedTimeSlots
                (Session.getUserId newSession)
                (Session.getOffset newSession)
            )

        Nothing ->
            ( model, Cmd.none )


setTimeSlotsElement :
    TS.WithTimeSlotsElement a
    -> Result Dom.Error Dom.Element
    -> ( TS.WithTimeSlotsElement a, Cmd msg )
setTimeSlotsElement model result =
    case result of
        Ok { element } ->
            ( { model | timeSlotsElement = Just element }, Cmd.none )

        Err _ ->
            ( model, Cmd.none )


setSavedWeeklyTimeSlots :
    TS.WithLoadingTimeSlots (TS.WithTimeSlotPositions (TS.WithSelectedTimeSlots a))
    -> Result Http.Error (List TSMessaging.ServerTimeSlot)
    -> ( TS.WithLoadingTimeSlots (TS.WithTimeSlotPositions (TS.WithSelectedTimeSlots a)), Cmd msg )
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
                { model
                    | loadingTimeSlots = False
                    , selectedTimeSlots = []
                }
                timeSlotList
            , Cmd.none
            )

        Err _ ->
            ( model, Cmd.none )


startSelectingTimeSlot :
    TS.WithSelectedTimeSlots (TS.WithTimeSlotPositions (TS.WithTimeSlotSelection a))
    -> TS.DayNum
    -> TS.SlotNum
    -> ( TS.WithSelectedTimeSlots (TS.WithTimeSlotPositions (TS.WithTimeSlotSelection a)), Cmd msg )
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


handleTimeSlotMouseMove :
    TS.WithTimeSlotSelection a
    -> (TS.PointerPosition -> Result Dom.Error Dom.Viewport -> msg)
    -> TS.PointerPosition
    -> ( TS.WithTimeSlotSelection a, Cmd msg )
handleTimeSlotMouseMove model adjustTSSelection pointerPosition =
    let
        adjustTSSelectionMsg =
            Task.attempt (adjustTSSelection pointerPosition) (Dom.getViewportOf TS.scrollableTimeSlotsId)
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
    -> ( TS.WithTimeSlotsElement (TS.WithSelectedTimeSlots (TS.WithTimeSlotPositions (TS.WithTimeSlotSelection a))), Cmd msg )
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


sendSaveTimeSlotRequest :
    TS.WithTimeSlotSelection (WithSession a)
    -> (Result Http.Error Int -> msg)
    -> ( TS.WithTimeSlotSelection (WithSession a), Cmd msg )
sendSaveTimeSlotRequest model setTSAfterSave =
    case model.timeSlotSelection of
        TS.CurrentlySelecting selectionBounds ->
            let
                { dayNum, startBound, endBound } =
                    TS.getOrderedTimeSlot selectionBounds
            in
            ( model
            , saveWeeklyTimeSlot setTSAfterSave
                (Session.getUserId model.session)
                (Session.getOffset model.session)
                dayNum
                startBound.slotNum
                endBound.slotNum
            )

        _ ->
            ( model, Cmd.none )


sendUpdateTimeSlotRequest :
    TS.WithTimeSlotSelection (WithSession a)
    -> (Result Http.Error NoData -> msg)
    -> ( TS.WithTimeSlotSelection (WithSession a), Cmd msg )
sendUpdateTimeSlotRequest model setTSAfterEdit =
    case model.timeSlotSelection of
        TS.EditingSelection selectionBounds prevSelection ->
            let
                { dayNum, startBound, endBound } =
                    TS.getOrderedTimeSlot selectionBounds

                timeSlotId =
                    .id <| TS.getTimeSlotFromDetails prevSelection
            in
            ( model
            , updateWeeklyTimeSlot setTSAfterEdit
                timeSlotId
                (Session.getOffset model.session)
                dayNum
                startBound.slotNum
                endBound.slotNum
            )

        _ ->
            ( model, Cmd.none )


setSelectedTimeSlotAfterCreation :
    EC.WithEventCreation (TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection a))
    -> Result Http.Error Int
    -> ( EC.WithEventCreation (TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection a)), Cmd msg )
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
    -> ( EC.WithEventCreation (TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection a)), Cmd msg )
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
    -> (Result Dom.Error Dom.Element -> msg)
    -> TS.DayNum
    -> TS.SlotNum
    -> ( TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection (TS.WithTimeSlotPositions a)), Cmd msg )
setOneHourSelection model promptEventDetails dayNum slotNum =
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
                        promptEventDetails

                ( _, _ ) ->
                    ( model, Cmd.none )

        Nothing ->
            ( { model | timeSlotSelection = TS.NotSelecting }, Cmd.none )


handleTimeSlotMouseUp :
    TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection (TS.WithTimeSlotPositions a))
    -> (Result Dom.Error Dom.Element -> msg)
    -> ( TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection (TS.WithTimeSlotPositions a)), Cmd msg )
handleTimeSlotMouseUp model promptEventDetails =
    case model.timeSlotSelection of
        TS.InitialPressNoMove { dayNum, startBound } ->
            setOneHourSelection model promptEventDetails dayNum startBound.slotNum

        TS.CurrentlySelecting _ ->
            ECUpdate.initiateUserPromptForEventDetails model promptEventDetails

        _ ->
            ( model, Cmd.none )


editTimeSlotSelection :
    TS.WithTimeSlotSelection (TS.WithSelectedTimeSlots a)
    -> (Result Dom.Error Dom.Element -> msg)
    -> TS.SelectedTimeSlotDetails
    -> ( TS.WithTimeSlotSelection (TS.WithSelectedTimeSlots a), Cmd msg )
editTimeSlotSelection model promptEventDetails selectedTimeSlotDetails =
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
        promptEventDetails


sendDeleteTimeSlotRequest :
    TS.WithTimeSlotSelection a
    -> (Result Http.Error NoData -> msg)
    -> ( TS.WithTimeSlotSelection a, Cmd msg )
sendDeleteTimeSlotRequest model onDelete =
    case model.timeSlotSelection of
        TS.EditingSelection _ prevSelection ->
            let
                timeSlotId =
                    .id <| TS.getTimeSlotFromDetails prevSelection
            in
            ( model
            , deleteWeeklyTimeSlot onDelete timeSlotId
            )

        _ ->
            ( model, Cmd.none )


deleteTimeSlot :
    EC.WithDiscardConfirmationModal (EC.WithEventCreation (TS.WithTimeSlotSelection a))
    -> Result Http.Error NoData
    -> ( EC.WithDiscardConfirmationModal (EC.WithEventCreation (TS.WithTimeSlotSelection a)), Cmd msg )
deleteTimeSlot model result =
    case result of
        Ok _ ->
            ECUpdate.closeUserPromptForEventDetails model

        _ ->
            ( model, Cmd.none )
