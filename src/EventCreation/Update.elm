module EventCreation.Update exposing
    ( adjustEventDescription
    , adjustEventTitle
    , cancelDiscardConfirmationModal
    , changeSelectionDayNum
    , changeSelectionEndSlot
    , changeSelectionStartSlot
    , closeUserPromptForEventDetails
    , handleEditingCancel
    , initiateUserPromptForEventDetails
    , promptUserForEventDetails
    , saveEditingTimeSlotWithoutChanges
    )

import Browser.Dom as Dom
import EventCreation.EventCreation as EC
import Task
import TimeSlots.TimeSlots as TS
import Utils exposing (applicative, getListItemAt)
import WeeklyFreeTimes.MainMsg exposing (Msg(..))


initiateUserPromptForEventDetails : TS.WithTimeSlotSelection a -> ( TS.WithTimeSlotSelection a, Cmd Msg )
initiateUserPromptForEventDetails model =
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

        TS.EditingSelection { dayNum, startBound, endBound } _ ->
            let
                minSlotNum =
                    min startBound.slotNum endBound.slotNum
            in
            ( model
            , Task.attempt
                PromptUserForEventDetails
                (Dom.getElement (TS.getTimeSlotId dayNum minSlotNum))
            )

        _ ->
            ( model, Cmd.none )


promptUserForEventDetails : EC.WithEventCreation a -> Result Dom.Error Dom.Element -> ( EC.WithEventCreation a, Cmd Msg )
promptUserForEventDetails model result =
    case result of
        Ok { viewport, element } ->
            let
                leftSpace =
                    element.x - viewport.x

                promptMargin =
                    12

                promptSpace =
                    EC.eventDetailsPromptWidth + 2 * promptMargin

                x =
                    if leftSpace >= promptSpace then
                        element.x - EC.eventDetailsPromptWidth - promptMargin

                    else
                        element.x + element.width + promptMargin
            in
            ( { model
                | eventCreation =
                    EC.CurrentlyCreatingEvent EC.WeeklyFreeTimes { x = x, y = element.y }
              }
            , Cmd.none
            )

        Err _ ->
            ( model, Cmd.none )


adjustEventTitle : EC.WithEventCreation a -> String -> ( EC.WithEventCreation a, Cmd Msg )
adjustEventTitle model title =
    case model.eventCreation of
        EC.CurrentlyCreatingEvent eventCreationDetails eventPosition ->
            case eventCreationDetails of
                EC.EventDetails eventDetails ->
                    ( { model
                        | eventCreation =
                            EC.CurrentlyCreatingEvent
                                (EC.EventDetails
                                    { eventDetails
                                        | title = title
                                    }
                                )
                                eventPosition
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        EC.NotCreating ->
            ( model, Cmd.none )


adjustEventDescription : EC.WithEventCreation a -> String -> ( EC.WithEventCreation a, Cmd Msg )
adjustEventDescription model description =
    case model.eventCreation of
        EC.CurrentlyCreatingEvent eventCreationDetails eventPosition ->
            case eventCreationDetails of
                EC.EventDetails eventDetails ->
                    ( { model
                        | eventCreation =
                            EC.CurrentlyCreatingEvent
                                (EC.EventDetails
                                    { eventDetails
                                        | description = description
                                    }
                                )
                                eventPosition
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        EC.NotCreating ->
            ( model, Cmd.none )


changeSelectionDayNum : TS.WithTimeSlotSelection a -> String -> ( TS.WithTimeSlotSelection a, Cmd Msg )
changeSelectionDayNum model dayNumStr =
    let
        maybeDayNum =
            String.toInt dayNumStr
    in
    case ( model.timeSlotSelection, maybeDayNum ) of
        ( TS.CurrentlySelecting selectionBounds, Just dayNum ) ->
            initiateUserPromptForEventDetails
                { model
                    | timeSlotSelection =
                        TS.CurrentlySelecting
                            { selectionBounds
                                | dayNum = dayNum
                            }
                }

        ( TS.EditingSelection selectionBounds previousDetails, Just dayNum ) ->
            initiateUserPromptForEventDetails
                { model
                    | timeSlotSelection =
                        TS.EditingSelection
                            { selectionBounds
                                | dayNum = dayNum
                            }
                            previousDetails
                }

        ( _, _ ) ->
            ( model, Cmd.none )


changeSelectionStartSlot :
    TS.WithTimeSlotPositions (TS.WithTimeSlotSelection a)
    -> String
    -> ( TS.WithTimeSlotPositions (TS.WithTimeSlotSelection a), Cmd Msg )
changeSelectionStartSlot model startSlotStr =
    let
        maybeStartSlotNum =
            String.toInt startSlotStr

        updateFunc { startBound, endBound, dayNum } startSlotNum =
            let
                startSlotDiff =
                    startSlotNum - startBound.slotNum

                shiftedEndSlot =
                    min TS.maxSlotNum <| endBound.slotNum + startSlotDiff

                newStartBound =
                    getListItemAt startSlotNum model.timeSlotPositions

                newEndBound =
                    getListItemAt shiftedEndSlot model.timeSlotPositions
            in
            Maybe.withDefault ( model, Cmd.none ) <|
                Maybe.map initiateUserPromptForEventDetails <|
                    applicative
                        (Maybe.map (TS.useTSPositionsForSelectionBounds model dayNum) newStartBound)
                        newEndBound
    in
    case model.timeSlotSelection of
        TS.CurrentlySelecting selectionBounds ->
            Maybe.withDefault ( model, Cmd.none ) <|
                Maybe.map (updateFunc selectionBounds) maybeStartSlotNum

        TS.EditingSelection selectionBounds _ ->
            Maybe.withDefault ( model, Cmd.none ) <|
                Maybe.map (updateFunc selectionBounds) maybeStartSlotNum

        _ ->
            ( model, Cmd.none )


changeSelectionEndSlot :
    TS.WithTimeSlotPositions (TS.WithTimeSlotSelection a)
    -> String
    -> ( TS.WithTimeSlotPositions (TS.WithTimeSlotSelection a), Cmd Msg )
changeSelectionEndSlot model endSlotStr =
    let
        maybeEndSlotNum =
            String.toInt endSlotStr
    in
    case ( model.timeSlotSelection, maybeEndSlotNum ) of
        ( TS.CurrentlySelecting { startBound, dayNum }, Just endSlotNum ) ->
            let
                newEndBound =
                    getListItemAt endSlotNum model.timeSlotPositions
            in
            ( Maybe.withDefault model <|
                Maybe.map (TS.useTSPositionsForSelectionBounds model dayNum startBound) newEndBound
            , Cmd.none
            )

        ( TS.EditingSelection { startBound, dayNum } _, Just endSlotNum ) ->
            let
                newEndBound =
                    getListItemAt endSlotNum model.timeSlotPositions
            in
            ( Maybe.withDefault model <|
                Maybe.map (TS.useTSPositionsForSelectionBounds model dayNum startBound) newEndBound
            , Cmd.none
            )

        ( _, _ ) ->
            ( model, Cmd.none )


handleEditingCancel :
    TS.WithSelectedTimeSlots (EC.WithDiscardConfirmationModal (EC.WithEventCreation (TS.WithTimeSlotSelection a)))
    -> ( TS.WithSelectedTimeSlots (EC.WithDiscardConfirmationModal (EC.WithEventCreation (TS.WithTimeSlotSelection a))), Cmd Msg )
handleEditingCancel model =
    case ( model.eventCreation, model.timeSlotSelection ) of
        ( EC.CurrentlyCreatingEvent eventCreationDetails _, TS.EditingSelection selectionBounds prevDetails ) ->
            let
                (TS.SelectedTimeSlotDetails prevSelectionBounds prevEventDetails) =
                    prevDetails

                areEventCreationsEqual =
                    EC.areEventCreationsEqual eventCreationDetails prevEventDetails

                areSelectionBoundsEqual =
                    TS.areSelectionBoundsEqual selectionBounds prevSelectionBounds
            in
            if not (areEventCreationsEqual && areSelectionBoundsEqual) then
                ( { model | isDiscardConfirmationModalOpen = True }, Cmd.none )

            else
                saveEditingTimeSlotWithoutChanges model

        ( _, _ ) ->
            closeUserPromptForEventDetails model


closeUserPromptForEventDetails :
    EC.WithDiscardConfirmationModal (EC.WithEventCreation (TS.WithTimeSlotSelection a))
    -> ( EC.WithDiscardConfirmationModal (EC.WithEventCreation (TS.WithTimeSlotSelection a)), Cmd Msg )
closeUserPromptForEventDetails model =
    ( { model
        | timeSlotSelection = TS.NotSelecting
        , eventCreation = EC.NotCreating
        , isDiscardConfirmationModalOpen = False
      }
    , Cmd.none
    )


cancelDiscardConfirmationModal : EC.WithDiscardConfirmationModal a -> ( EC.WithDiscardConfirmationModal a, Cmd Msg )
cancelDiscardConfirmationModal model =
    ( { model | isDiscardConfirmationModalOpen = False }, Cmd.none )


saveEditingTimeSlotWithoutChanges :
    TS.WithSelectedTimeSlots (EC.WithDiscardConfirmationModal (EC.WithEventCreation (TS.WithTimeSlotSelection a)))
    -> ( TS.WithSelectedTimeSlots (EC.WithDiscardConfirmationModal (EC.WithEventCreation (TS.WithTimeSlotSelection a))), Cmd Msg )
saveEditingTimeSlotWithoutChanges model =
    case model.timeSlotSelection of
        TS.EditingSelection _ prevDetails ->
            closeUserPromptForEventDetails
                { model
                    | selectedTimeSlots =
                        prevDetails :: model.selectedTimeSlots
                }

        _ ->
            ( model, Cmd.none )