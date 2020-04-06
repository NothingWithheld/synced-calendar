module EventCreation.Update exposing
    ( adjustEventDescription
    , adjustEventTitle
    , closeUserPromptForEventDetails
    , initiateUserPromptForEventDetails
    , promptUserForEventDetails
    )

import Browser.Dom as Dom
import EventCreation.EventCreation as EC
import MainMsg exposing (Msg(..))
import Task
import TimeSlots.TimeSlots as TS


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
                    EC.CurrentlyCreatingEvent { title = "", description = "" } { x = x, y = element.y }
              }
            , Cmd.none
            )

        Err _ ->
            ( model, Cmd.none )


adjustEventTitle : EC.WithEventCreation a -> String -> ( EC.WithEventCreation a, Cmd Msg )
adjustEventTitle model title =
    case model.eventCreation of
        EC.CurrentlyCreatingEvent eventDetails eventPosition ->
            ( { model
                | eventCreation =
                    EC.CurrentlyCreatingEvent
                        { eventDetails
                            | title = title
                        }
                        eventPosition
              }
            , Cmd.none
            )

        EC.NotCreating ->
            ( model, Cmd.none )


adjustEventDescription : EC.WithEventCreation a -> String -> ( EC.WithEventCreation a, Cmd Msg )
adjustEventDescription model description =
    case model.eventCreation of
        EC.CurrentlyCreatingEvent eventDetails eventPosition ->
            ( { model
                | eventCreation =
                    EC.CurrentlyCreatingEvent
                        { eventDetails
                            | description = description
                        }
                        eventPosition
              }
            , Cmd.none
            )

        EC.NotCreating ->
            ( model, Cmd.none )


closeUserPromptForEventDetails : EC.WithEventCreation (TS.WithTimeSlotSelection a) -> ( EC.WithEventCreation (TS.WithTimeSlotSelection a), Cmd Msg )
closeUserPromptForEventDetails model =
    ( { model | timeSlotSelection = TS.NotSelecting, eventCreation = EC.NotCreating }, Cmd.none )
