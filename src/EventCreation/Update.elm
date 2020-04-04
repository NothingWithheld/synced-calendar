module EventCreation.Update exposing (update)

import Browser.Dom as Dom
import EventCreation.EventCreation as EC
import Task
import TimeSlots.TimeSlots as TS
import Utils exposing (getListItemAt)


type alias WithEventCreationEverything a =
    EC.WithEventCreation (TS.WithTimeSlotPositions (TS.WithTimeSlotSelection a))


update : EC.Msg -> WithEventCreationEverything a -> ( WithEventCreationEverything a, Cmd EC.Msg )
update msg model =
    case msg of
        EC.SetOneHourSelection dayNum slotNum ->
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
                    update EC.InitiateUserPromptForEventDetails
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

        EC.InitiateUserPromptForEventDetails ->
            case ( model.eventCreation, model.timeSlotSelection ) of
                ( EC.NotCreating, TS.CurrentlySelecting { dayNum, startBound, endBound } ) ->
                    let
                        minSlotNum =
                            min startBound.slotNum endBound.slotNum
                    in
                    ( model
                    , Task.attempt
                        EC.PromptUserForEventDetails
                        (Dom.getElement (TS.getTimeSlotId dayNum minSlotNum))
                    )

                ( _, _ ) ->
                    ( model, Cmd.none )

        EC.PromptUserForEventDetails result ->
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

        EC.AdjustEventTitle title ->
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

        EC.AdjustEventDescription description ->
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

        EC.CloseUserPromptForEventDetails ->
            ( { model | timeSlotSelection = TS.NotSelecting, eventCreation = EC.NotCreating }, Cmd.none )
