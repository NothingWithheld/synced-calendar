module TimeSlots.Update exposing
    ( adjustTimeSlotSelection
    , deleteTimeSlot
    , editTimeSlotSelection
    , handleTimeSlotMouseMove
    , handleTimeSlotMouseUp
    , moveWeekBackward
    , moveWeekForward
    , sendDeleteTimeSlotRequest
    , sendSaveTimeSlotRequest
    , sendUpdateTimeSlotRequest
    , setInitialTime
    , setSavedConfirmedEventsBy
    , setSavedConfirmedEventsFor
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
import ProposeEvent.ProposeEvent as PE
import Session exposing (WithSession)
import Task
import Time exposing (Posix)
import TimeSlots.Commands
    exposing
        ( deleteWeeklyTimeSlot
        , requestConfirmedEventsBy
        , requestConfirmedEventsFor
        , requestSavedWeeklyTimeSlots
        , saveWeeklyTimeSlot
        , updateWeeklyTimeSlot
        )
import TimeSlots.Messaging as TSMessaging
import TimeSlots.Time as TSTime exposing (TimeDetails(..))
import TimeSlots.TimeSlots as TS exposing (Calendar(..))
import Utils
    exposing
        ( NoData
        , applyTwice
        , defaultOnError
        , defaultWithoutData
        , getListItemAt
        , useWithoutCmdMsg
        )



-- time


setInitialTime :
    TSTime.WithTimeDetails (PE.WithProposedEvent (WithSession a))
    -> Calendar b c d
    -> Result Never Posix
    -> ( TSTime.WithTimeDetails (PE.WithProposedEvent (WithSession a)), Cmd msg )
setInitialTime model updates result =
    case result of
        Ok currentDay ->
            case ( updates, model.proposedEvent ) of
                ( SubmitAvailability _, Just { fromDate } ) ->
                    ( { model
                        | timeDetails =
                            WithTime
                                { currentDay = currentDay
                                , weekOffset = TSTime.shiftWeeksToStartDate model currentDay fromDate
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model
                        | timeDetails =
                            WithTime { currentDay = currentDay, weekOffset = 0 }
                      }
                    , Cmd.none
                    )

        Err _ ->
            ( model, Cmd.none )


moveWeekForward : TSTime.WithTimeDetails a -> ( TSTime.WithTimeDetails a, Cmd msg )
moveWeekForward model =
    case model.timeDetails of
        WithTime timeDetails ->
            ( { model
                | timeDetails =
                    WithTime
                        { timeDetails | weekOffset = timeDetails.weekOffset + 1 }
              }
            , Cmd.none
            )

        WithoutTime ->
            ( model, Cmd.none )


moveWeekBackward : TSTime.WithTimeDetails a -> ( TSTime.WithTimeDetails a, Cmd msg )
moveWeekBackward model =
    case model.timeDetails of
        WithTime timeDetails ->
            ( { model
                | timeDetails =
                    WithTime
                        { timeDetails | weekOffset = timeDetails.weekOffset - 1 }
              }
            , Cmd.none
            )

        WithoutTime ->
            ( model, Cmd.none )


updateTimeZone :
    TS.WithLoadingAllExceptTSPositions (WithSession (TS.WithSelectedTimeSlots a))
    ->
        Calendar (Result Http.Error (List TSMessaging.ServerTimeSlot) -> msg)
            { b
                | setSavedConfirmedEvBy :
                    Result Http.Error (List TSMessaging.ServerConfirmedEvent) -> msg
                , setSavedConfirmedEvFor :
                    Result Http.Error (List TSMessaging.ServerConfirmedEvent) -> msg
            }
            c
    -> String
    -> ( TS.WithLoadingAllExceptTSPositions (WithSession (TS.WithSelectedTimeSlots a)), Cmd msg )
updateTimeZone model updates timeZoneLabel =
    case Session.setOffset model.session timeZoneLabel of
        Just newSession ->
            ( { model
                | session = newSession
                , selectedTimeSlots = []
                , loadingWeeklyFreeTimes =
                    case updates of
                        WeeklyFreeTimes _ ->
                            True

                        Events _ ->
                            False

                        SubmitAvailability _ ->
                            True
                , loadingConfirmedEventsBy =
                    case updates of
                        WeeklyFreeTimes _ ->
                            False

                        Events _ ->
                            -- Server Bug -> sends same result for By and For
                            -- possibly sends results only to creator
                            False

                        SubmitAvailability _ ->
                            True
                , loadingConfirmedEventsFor =
                    case updates of
                        WeeklyFreeTimes _ ->
                            False

                        Events _ ->
                            True

                        SubmitAvailability _ ->
                            True
              }
            , case updates of
                WeeklyFreeTimes setSavedWeeklyTS ->
                    requestSavedWeeklyTimeSlots
                        setSavedWeeklyTS
                        (Session.getUserId newSession)
                        (Session.getOffset newSession)

                Events { setSavedConfirmedEvBy, setSavedConfirmedEvFor } ->
                    Cmd.batch
                        [ -- Server Bug -> sends same result for By and For
                          -- possibly sends results only to creator
                          -- requestConfirmedEventsBy
                          -- setSavedConfirmedEvBy
                          -- (Session.getUserId newSession)
                          -- (Session.getOffset newSession)
                          requestConfirmedEventsFor
                            setSavedConfirmedEvFor
                            (Session.getUserId newSession)
                            (Session.getOffset newSession)
                        ]

                SubmitAvailability _ ->
                    Cmd.none
            )

        Nothing ->
            ( model, Cmd.none )



-- DOM position loads


setTimeSlotPositions :
    TS.WithTimeSlotPositions (TS.WithLoadingTSPositions (WithSession a))
    ->
        Calendar (Result Http.Error (List TSMessaging.ServerTimeSlot) -> msg)
            { b
                | setSavedConfirmedEvBy :
                    Result Http.Error (List TSMessaging.ServerConfirmedEvent) -> msg
                , setSavedConfirmedEvFor :
                    Result Http.Error (List TSMessaging.ServerConfirmedEvent) -> msg
            }
            { b
                | setSavedConfirmedEvBy :
                    Result Http.Error (List TSMessaging.ServerConfirmedEvent) -> msg
                , setSavedConfirmedEvFor :
                    Result Http.Error (List TSMessaging.ServerConfirmedEvent) -> msg
                , setSavedWeeklyTS :
                    Result Http.Error (List TSMessaging.ServerTimeSlot) -> msg
            }
    -> Result Dom.Error (List Dom.Element)
    -> ( TS.WithTimeSlotPositions (TS.WithLoadingTSPositions (WithSession a)), Cmd msg )
setTimeSlotPositions model updates result =
    let
        updateModelWithTSPositions elementList =
            ( { model
                | timeSlotPositions = TS.setTimeSlotPositions TS.startingSlotNum 0 elementList
                , loadingTSPositions = False
              }
            , case updates of
                WeeklyFreeTimes setSavedWeeklyTS ->
                    requestSavedWeeklyTimeSlots
                        setSavedWeeklyTS
                        (Session.getUserId model.session)
                        (Session.getOffset model.session)

                Events { setSavedConfirmedEvBy, setSavedConfirmedEvFor } ->
                    Cmd.batch
                        [ -- Server Bug -> sends same result for By and For
                          -- possibly sends results only to creator
                          -- requestConfirmedEventsBy
                          -- setSavedConfirmedEvBy
                          -- (Session.getUserId model.session)
                          -- (Session.getOffset model.session)
                          requestConfirmedEventsFor
                            setSavedConfirmedEvFor
                            (Session.getUserId model.session)
                            (Session.getOffset model.session)
                        ]

                SubmitAvailability { setSavedWeeklyTS, setSavedConfirmedEvBy, setSavedConfirmedEvFor } ->
                    Cmd.batch
                        [ -- Server Bug -> sends same result for By and For
                          -- possibly sends results only to creator
                          -- requestConfirmedEventsBy
                          -- setSavedConfirmedEvBy
                          -- (Session.getUserId model.session)
                          -- (Session.getOffset model.session)
                          requestConfirmedEventsFor
                            setSavedConfirmedEvFor
                            (Session.getUserId model.session)
                            (Session.getOffset model.session)
                        , requestSavedWeeklyTimeSlots
                            setSavedWeeklyTS
                            (Session.getUserId model.session)
                            (Session.getOffset model.session)
                        ]
            )
    in
    defaultOnError ( model, Cmd.none ) result updateModelWithTSPositions


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



-- saved results from DB


setSavedWeeklyTimeSlots :
    TS.WithLoadingAllExceptTSPositions (TS.WithTimeSlotPositions (TS.WithSelectedTimeSlots (PE.WithProposedEvent a)))
    -> Calendar b c d
    -> Result Http.Error (List TSMessaging.ServerTimeSlot)
    -> ( TS.WithLoadingAllExceptTSPositions (TS.WithTimeSlotPositions (TS.WithSelectedTimeSlots (PE.WithProposedEvent a))), Cmd msg )
setSavedWeeklyTimeSlots model updates result =
    let
        updateWithTimeSlot { dayNum, startSlot, endSlot, id } model_ =
            let
                startBound =
                    getListItemAt startSlot model_.timeSlotPositions

                endBound =
                    getListItemAt endSlot model_.timeSlotPositions
            in
            case Maybe.map2 (TS.TimeSlot dayNum) startBound endBound of
                Just selectionBounds ->
                    { model_
                        | selectedTimeSlots =
                            TS.SelectedTimeSlotDetails selectionBounds (EC.SetWeeklyFreeTime id)
                                :: model_.selectedTimeSlots
                    }

                Nothing ->
                    model_
    in
    case result of
        Ok timeSlotList ->
            let
                updatedModel =
                    List.foldl
                        updateWithTimeSlot
                        { model
                            | loadingWeeklyFreeTimes = False
                        }
                        timeSlotList
            in
            case updates of
                SubmitAvailability _ ->
                    if TS.isFinishedLoadingForAvailableEvents updatedModel then
                        setAvailableTimesFromWFT updatedModel

                    else
                        ( updatedModel, Cmd.none )

                _ ->
                    ( updatedModel, Cmd.none )

        Err _ ->
            ( model, Cmd.none )


updateWithConfirmedEvent :
    TSMessaging.ServerConfirmedEvent
    -> TS.WithTimeSlotPositions (TS.WithSelectedTimeSlots a)
    -> TS.WithTimeSlotPositions (TS.WithSelectedTimeSlots a)
updateWithConfirmedEvent { eventId, recipientIds, creatorId, title, description, date, dayNum, startSlot, endSlot } model =
    let
        startBound =
            getListItemAt startSlot model.timeSlotPositions

        endBound =
            getListItemAt endSlot model.timeSlotPositions
    in
    case Maybe.map2 (TS.TimeSlot dayNum) startBound endBound of
        Just selectionBounds ->
            { model
                | selectedTimeSlots =
                    TS.SelectedTimeSlotDetails selectionBounds
                        (EC.ConfirmedEvent
                            { eventId = eventId
                            , recipientIds = recipientIds
                            , creatorId = creatorId
                            , title = title
                            , description = description
                            , date = date
                            }
                        )
                        :: model.selectedTimeSlots
            }

        Nothing ->
            model


setSavedConfirmedEventsFor :
    TS.WithLoadingAllExceptTSPositions (TS.WithTimeSlotPositions (TS.WithSelectedTimeSlots (PE.WithProposedEvent a)))
    -> Calendar b c d
    -> Result Http.Error (List TSMessaging.ServerConfirmedEvent)
    -> ( TS.WithLoadingAllExceptTSPositions (TS.WithTimeSlotPositions (TS.WithSelectedTimeSlots (PE.WithProposedEvent a))), Cmd msg )
setSavedConfirmedEventsFor model updates result =
    case result of
        Ok confirmedEventList ->
            let
                updatedModel =
                    List.foldl
                        updateWithConfirmedEvent
                        { model
                            | loadingConfirmedEventsFor = False
                        }
                        confirmedEventList
            in
            case updates of
                SubmitAvailability _ ->
                    if TS.isFinishedLoadingForAvailableEvents updatedModel then
                        setAvailableTimesFromWFT updatedModel

                    else
                        ( updatedModel, Cmd.none )

                _ ->
                    ( updatedModel, Cmd.none )

        Err _ ->
            ( model, Cmd.none )


setSavedConfirmedEventsBy :
    TS.WithLoadingAllExceptTSPositions (TS.WithTimeSlotPositions (TS.WithSelectedTimeSlots (PE.WithProposedEvent a)))
    -> Calendar b c d
    -> Result Http.Error (List TSMessaging.ServerConfirmedEvent)
    -> ( TS.WithLoadingAllExceptTSPositions (TS.WithTimeSlotPositions (TS.WithSelectedTimeSlots (PE.WithProposedEvent a))), Cmd msg )
setSavedConfirmedEventsBy model updates result =
    case result of
        Ok confirmedEventList ->
            let
                updatedModel =
                    List.foldl
                        updateWithConfirmedEvent
                        { model
                            | loadingConfirmedEventsBy = False
                        }
                        confirmedEventList
            in
            case updates of
                SubmitAvailability _ ->
                    if TS.isFinishedLoadingForAvailableEvents updatedModel then
                        setAvailableTimesFromWFT updatedModel

                    else
                        ( updatedModel, Cmd.none )

                _ ->
                    ( updatedModel, Cmd.none )

        Err _ ->
            ( model, Cmd.none )



-- Set available times from weekly free times


setAvailableTimesFromWFT :
    TS.WithTimeSlotPositions (PE.WithProposedEvent (TS.WithSelectedTimeSlots (TS.WithLoadingAvailableTimes a)))
    -> ( TS.WithTimeSlotPositions (PE.WithProposedEvent (TS.WithSelectedTimeSlots (TS.WithLoadingAvailableTimes a))), Cmd msg )
setAvailableTimesFromWFT model =
    let
        isConfirmedEvent eventDetails =
            case eventDetails of
                EC.ConfirmedEvent _ ->
                    True

                _ ->
                    False

        isFreeTime eventDetails =
            case eventDetails of
                EC.SetWeeklyFreeTime _ ->
                    True

                _ ->
                    False

        confirmedEventTimeSlots =
            List.filter
                (isConfirmedEvent << TS.getEventDetailsFromDetails)
                model.selectedTimeSlots

        weeklyFreeTimeTimeSlots =
            List.filter
                (isFreeTime << TS.getEventDetailsFromDetails)
                model.selectedTimeSlots

        foldFunc date availableTimeSlots =
            let
                dayNumForDate =
                    TSTime.dateToDayNum date

                wfTimesForDayNum =
                    List.filter
                        ((==) dayNumForDate << .dayNum << TS.getTimeSlotFromDetails)
                        weeklyFreeTimeTimeSlots

                nonConflictingTSs =
                    List.concatMap
                        (TS.getNonConflictingPartsOfTimeSlot model confirmedEventTimeSlots << TS.getTimeSlotFromDetails)
                        wfTimesForDayNum
            in
            List.map
                (\timeSlot -> TS.SelectedTimeSlotDetails timeSlot (EC.AvailableTime date))
                nonConflictingTSs
                ++ availableTimeSlots
    in
    case model.proposedEvent of
        Just { fromDate, toDate } ->
            let
                availableTimeSlots =
                    List.foldl foldFunc [] <| TSTime.daysFrom fromDate toDate
            in
            ( { model
                | selectedTimeSlots = availableTimeSlots ++ model.selectedTimeSlots
                , loadingAvailableTimes = False
              }
            , Cmd.none
            )

        Nothing ->
            ( model, Cmd.none )



-- time slot user interaction


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
            in
            case TS.getEventDetailsFromDetails prevSelection of
                EC.SetWeeklyFreeTime timeSlotId ->
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

        _ ->
            ( model, Cmd.none )


setSelectedTimeSlotAfterCreation :
    EC.WithEventCreation (TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection a))
    -> Result Http.Error Int
    -> ( EC.WithEventCreation (TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection a)), Cmd msg )
setSelectedTimeSlotAfterCreation model result =
    case ( model.eventCreation, model.timeSlotSelection, result ) of
        ( EC.CurrentlyCreatingEvent _ _, TS.CurrentlySelecting selectionBounds, Ok timeSlotId ) ->
            let
                orderedSelectionBounds =
                    TS.getOrderedTimeSlot selectionBounds

                selectedTimeSlot =
                    TS.SelectedTimeSlotDetails
                        orderedSelectionBounds
                        (EC.SetWeeklyFreeTime timeSlotId)

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
        ( EC.CurrentlyCreatingEvent eventDetails _, TS.EditingSelection selectionBounds _, Ok _ ) ->
            let
                orderedSelectionBounds =
                    TS.getOrderedTimeSlot selectionBounds

                selectedTimeSlot =
                    TS.SelectedTimeSlotDetails
                        orderedSelectionBounds
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
    -> (EC.EventDetails -> Result Dom.Error Dom.Element -> msg)
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
                        EC.UnsetWeeklyFreeTime
                        promptEventDetails

                ( _, _ ) ->
                    ( model, Cmd.none )

        Nothing ->
            ( { model | timeSlotSelection = TS.NotSelecting }, Cmd.none )


handleTimeSlotMouseUp :
    TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection (TS.WithTimeSlotPositions a))
    -> (EC.EventDetails -> Result Dom.Error Dom.Element -> msg)
    -> ( TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection (TS.WithTimeSlotPositions a)), Cmd msg )
handleTimeSlotMouseUp model promptEventDetails =
    case model.timeSlotSelection of
        TS.InitialPressNoMove { dayNum, startBound } ->
            setOneHourSelection model promptEventDetails dayNum startBound.slotNum

        TS.CurrentlySelecting _ ->
            ECUpdate.initiateUserPromptForEventDetails model EC.UnsetWeeklyFreeTime promptEventDetails

        _ ->
            ( model, Cmd.none )


editTimeSlotSelection :
    TS.WithTimeSlotSelection (TS.WithSelectedTimeSlots a)
    -> (EC.EventDetails -> Result Dom.Error Dom.Element -> msg)
    -> TS.SelectedTimeSlotDetails
    -> ( TS.WithTimeSlotSelection (TS.WithSelectedTimeSlots a), Cmd msg )
editTimeSlotSelection model promptEventDetails selectedTimeSlotDetails =
    let
        (TS.SelectedTimeSlotDetails selectedTimeSlot eventDetails) =
            selectedTimeSlotDetails

        selectedTimeSlotsWithoutChosen =
            List.filter ((/=) selectedTimeSlotDetails) model.selectedTimeSlots
    in
    ECUpdate.initiateUserPromptForEventDetails
        { model
            | timeSlotSelection =
                TS.EditingSelection selectedTimeSlot selectedTimeSlotDetails
            , selectedTimeSlots = selectedTimeSlotsWithoutChosen
        }
        eventDetails
        promptEventDetails


sendDeleteTimeSlotRequest :
    TS.WithTimeSlotSelection a
    -> (Result Http.Error NoData -> msg)
    -> ( TS.WithTimeSlotSelection a, Cmd msg )
sendDeleteTimeSlotRequest model onDelete =
    case model.timeSlotSelection of
        TS.EditingSelection _ prevSelection ->
            case TS.getEventDetailsFromDetails prevSelection of
                EC.SetWeeklyFreeTime timeSlotId ->
                    ( model
                    , deleteWeeklyTimeSlot onDelete timeSlotId
                    )

                _ ->
                    ( model, Cmd.none )

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
