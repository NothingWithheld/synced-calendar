module Pages.EventCalendar exposing (Model, Msg, init, subscriptions, update, view)

import AvailableTime.AvailableTime exposing (AvailableTimeDetails)
import Browser exposing (Document)
import Browser.Dom as Dom
import EventCreation.EventCreation as EC
import EventCreation.Update as ECUpdate
import EventCreation.View exposing (viewDiscardConfirmationModal, viewUserRequest)
import Html exposing (div)
import Http
import Material
import Material.Options exposing (css, styled)
import ProposeEvent.ProposeEvent exposing (ProposedEvent)
import Session exposing (Session)
import Time exposing (Posix)
import TimeSlots.Commands
    exposing
        ( requestCurrentDay
        , requestTimeSlotPositions
        , requestTimeSlotsElement
        )
import TimeSlots.Messaging as TSMessaging
import TimeSlots.Time as TSTime
import TimeSlots.TimeSlots as TS exposing (Calendar(..))
import TimeSlots.Update as TSUpdate
import TimeSlots.View exposing (viewCalendarHeading, viewDayHeadings, viewScrollableTimeSlots)



-- MODEL


type alias Model =
    { session : Session
    , timeDetails : TSTime.TimeDetails
    , loadingWeeklyFreeTimes : Bool
    , loadingConfirmedEventsBy : Bool
    , loadingConfirmedEventsFor : Bool
    , loadingAvailableTimes : Bool
    , loadingTSPositions : Bool
    , loadingAvailableTimesCount : Bool
    , loadingAvailabilityMap : Bool
    , timeSlotPositions : List TS.TimeSlotBoundaryPosition
    , timeSlotsElement : Maybe TS.Element
    , timeSlotSelection : TS.TimeSlotSelection
    , eventCreation : EC.EventCreation
    , selectedTimeSlots : List TS.SelectedTimeSlotDetails
    , isDiscardConfirmationModalOpen : Bool
    , mdc : Material.Model Msg
    , proposedEvent : Maybe ProposedEvent
    , alreadySubmittedAvailability : Bool
    , totalRecipients : Int
    , countSubmitted : Int
    , availabilityMap : List AvailableTimeDetails
    , alreadySubmittedConfirmedEvent : Bool
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , timeDetails = Nothing
      , loadingWeeklyFreeTimes = False
      , loadingConfirmedEventsBy = True
      , loadingConfirmedEventsFor = True
      , loadingAvailableTimes = False
      , loadingTSPositions = True
      , loadingAvailableTimesCount = False
      , loadingAvailabilityMap = False
      , timeSlotPositions = []
      , timeSlotsElement = Nothing
      , timeSlotSelection = TS.NotSelecting
      , eventCreation = EC.NotCreating
      , selectedTimeSlots = []
      , isDiscardConfirmationModalOpen = False
      , mdc = Material.defaultModel
      , proposedEvent = Nothing
      , alreadySubmittedAvailability = False
      , totalRecipients = 0
      , countSubmitted = 0
      , availabilityMap = []
      , alreadySubmittedConfirmedEvent = False
      }
    , Cmd.batch
        [ Material.init Mdc
        , requestTimeSlotPositions SetTimeSlotPositions
        , requestTimeSlotsElement SetTimeSlotsElement
        , requestCurrentDay SetInitialTime
        ]
    )



-- UPDATE


type Msg
    = NoOp
      -- TimeSlots
    | SetInitialTime (Result Never Posix)
    | MoveWeekForward
    | MoveWeekBackward
    | SetTimeSlotPositions (Result Dom.Error (List Dom.Element))
    | SetSavedConfirmedEventsBy (Result Http.Error (List TSMessaging.ServerConfirmedEvent))
    | SetSavedConfirmedEventsFor (Result Http.Error (List TSMessaging.ServerConfirmedEvent))
    | UpdateTimeZone String
    | SetTimeSlotsElement (Result Dom.Error Dom.Element)
    | EditTimeSlotSelection TS.SelectedTimeSlotDetails
      -- EventCreation
    | PromptUserForEventDetails EC.EventDetails (Result Dom.Error Dom.Element)
    | HandleEditingCancel
    | CancelDiscardConfirmationModal
    | SaveEditingTimeSlotWithoutChanges
    | Mdc (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Mdc msg_ ->
            Material.update Mdc msg_ model

        -- TimeSlots
        SetInitialTime result ->
            TSUpdate.setInitialTime model (Events {}) result

        MoveWeekForward ->
            TSUpdate.moveWeekForward model

        MoveWeekBackward ->
            TSUpdate.moveWeekBackward model

        SetTimeSlotPositions result ->
            TSUpdate.setTimeSlotPositions model
                (Events
                    { setSavedConfirmedEvBy = SetSavedConfirmedEventsBy
                    , setSavedConfirmedEvFor = SetSavedConfirmedEventsFor
                    }
                )
                result

        SetSavedConfirmedEventsBy result ->
            TSUpdate.setSavedConfirmedEventsBy model (Events {}) result

        SetSavedConfirmedEventsFor result ->
            TSUpdate.setSavedConfirmedEventsFor model (Events {}) result

        UpdateTimeZone timeZoneLabel ->
            TSUpdate.updateTimeZone model
                (Events
                    { setSavedConfirmedEvBy = SetSavedConfirmedEventsBy
                    , setSavedConfirmedEvFor = SetSavedConfirmedEventsFor
                    }
                )
                timeZoneLabel

        SetTimeSlotsElement result ->
            TSUpdate.setTimeSlotsElement model result

        EditTimeSlotSelection selectedTimeslotDetails ->
            TSUpdate.editTimeSlotSelection model PromptUserForEventDetails selectedTimeslotDetails

        -- EventCreation
        PromptUserForEventDetails eventDetails result ->
            ECUpdate.promptUserForEventDetails model eventDetails result

        HandleEditingCancel ->
            ECUpdate.handleEditingCancel model

        CancelDiscardConfirmationModal ->
            ECUpdate.cancelDiscardConfirmationModal model

        SaveEditingTimeSlotWithoutChanges ->
            ECUpdate.saveEditingTimeSlotWithoutChanges model



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Edit What Times You're Free Each Week"
    , body =
        [ styled div
            [ css "position" "relative"
            , css "display" "flex"
            , css "flex-direction" "column-reverse"
            , css "height" "100vh"
            ]
            [ styled div
                []
                [ viewCalendarHeading model
                    (Events
                        { onMdc = Mdc
                        , onTimeZoneSelect = UpdateTimeZone
                        , moveWeekBackward = MoveWeekBackward
                        , moveWeekForward = MoveWeekForward
                        }
                    )
                , viewDayHeadings model
                , viewScrollableTimeSlots model (Events { editTimeSlotSelection = EditTimeSlotSelection })
                ]
            , viewDiscardConfirmationModal model
                { cancelDiscardConfirmationModal = CancelDiscardConfirmationModal
                , onMdc = Mdc
                , saveEditingTimeSlotWithoutChanges = SaveEditingTimeSlotWithoutChanges
                }
            ]
        , viewUserRequest model
            (Events
                { onMdc = Mdc
                , handleEditingCancel = HandleEditingCancel
                , noOp = NoOp
                }
            )
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model
