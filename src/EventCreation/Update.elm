module EventCreation.Update exposing
    ( adjustEventDescription
    , adjustEventTitle
    , changeSelectionDayNum
    , changeSelectionEndSlot
    , changeSelectionStartSlot
    , closeUserPromptForEventDetails
    , initiateUserPromptForEventDetails
    , promptUserForEventDetails
    )

import Browser.Dom as Dom
import EventCreation.EventCreation as EC
import MainMsg exposing (Msg(..))
import Task
import TimeSlots.TimeSlots as TS
import Utils exposing (applicative, getListItemAt)


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
    in
    case ( model.timeSlotSelection, maybeStartSlotNum ) of
        ( TS.CurrentlySelecting { startBound, endBound, dayNum }, Just startSlotNum ) ->
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

        ( _, _ ) ->
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

        ( _, _ ) ->
            ( model, Cmd.none )


closeUserPromptForEventDetails : EC.WithEventCreation (TS.WithTimeSlotSelection a) -> ( EC.WithEventCreation (TS.WithTimeSlotSelection a), Cmd Msg )
closeUserPromptForEventDetails model =
    ( { model | timeSlotSelection = TS.NotSelecting, eventCreation = EC.NotCreating }, Cmd.none )
