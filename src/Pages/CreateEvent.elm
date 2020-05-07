module Pages.CreateEvent exposing (Model, Msg, init, subscriptions, update, view)

import AvailableTime.AvailableTime as AT exposing (AvailableTimeDetails)
import AvailableTime.Commands exposing (requestAllAvailableTimes, requestAvailableTimesCount)
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
import TimeSlots.Commands exposing (requestCurrentDay, requestTimeSlotPositions, requestTimeSlotsElement)
import TimeSlots.Messaging as TSMessaging
import TimeSlots.Time as TSTime
import TimeSlots.TimeSlots as TS exposing (Calendar(..))
import TimeSlots.Update as TSUpdate
import TimeSlots.View exposing (viewCalendarHeading, viewDayHeadings, viewScrollableTimeSlots)
import Utils exposing (NoData)



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
    }


init : Session -> ProposedEvent -> ( Model, Cmd Msg )
init session proposedEvent =
    ( { session = session
      , timeDetails = Nothing
      , loadingWeeklyFreeTimes = False

      -- Server Bug -> sends same result for By and For
      -- possibly sends results only to creator
      , loadingConfirmedEventsBy = False
      , loadingConfirmedEventsFor = True
      , loadingAvailableTimes = False
      , loadingTSPositions = True
      , loadingAvailableTimesCount = True
      , loadingAvailabilityMap = True
      , timeSlotPositions = []
      , timeSlotsElement = Nothing
      , timeSlotSelection = TS.NotSelecting
      , eventCreation = EC.NotCreating
      , selectedTimeSlots = []
      , isDiscardConfirmationModalOpen = False
      , mdc = Material.defaultModel
      , proposedEvent = Just proposedEvent
      , alreadySubmittedAvailability = False
      , totalRecipients = 0
      , countSubmitted = 0
      , availabilityMap = []
      }
    , Cmd.batch
        [ Material.init Mdc
        , requestTimeSlotPositions SetTimeSlotPositions
        , requestTimeSlotsElement SetTimeSlotsElement
        , requestCurrentDay SetInitialTime
        , requestAvailableTimesCount SetAvailableTimesCount proposedEvent.eventId
        , requestAllAvailableTimes { session = session } SetAvailabilityMap proposedEvent.eventId
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
    | SetAvailableTimesCount (Result Http.Error (Maybe AT.ServerAvailableTimesCount))
    | SetAvailabilityMap (Result Http.Error (List AvailableTimeDetails))
    | StartSelectingTimeSlot TS.DayNum TS.SlotNum
    | HandleTimeSlotMouseMove TS.PointerPosition
    | AdjustTimeSlotSelection TS.PointerPosition (Result Dom.Error Dom.Viewport)
    | HandleTimeSlotMouseUp
    | EditTimeSlotSelection TS.SelectedTimeSlotDetails
    | SaveAvailableTimeSlot
      -- EventCreation
    | PromptUserForEventDetails EC.EventDetails (Result Dom.Error Dom.Element)
    | ChangeSelectionDayNum String
    | ChangeSelectionStartSlot String
    | ChangeSelectionEndSlot String
    | HandleEditingCancel
    | CloseUserPromptForEventDetails
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
            TSUpdate.setInitialTime model (CreateEvent {}) result

        MoveWeekForward ->
            TSUpdate.moveWeekForward model

        MoveWeekBackward ->
            TSUpdate.moveWeekBackward model

        SetTimeSlotPositions result ->
            TSUpdate.setTimeSlotPositions model
                (CreateEvent
                    { setSavedConfirmedEvBy = SetSavedConfirmedEventsBy
                    , setSavedConfirmedEvFor = SetSavedConfirmedEventsFor
                    }
                )
                result

        SetSavedConfirmedEventsBy result ->
            TSUpdate.setSavedConfirmedEventsBy model
                (CreateEvent {})
                result

        SetSavedConfirmedEventsFor result ->
            TSUpdate.setSavedConfirmedEventsFor model
                (CreateEvent {})
                result

        UpdateTimeZone timeZoneLabel ->
            TSUpdate.updateTimeZone model
                (CreateEvent
                    { setSavedConfirmedEvBy = SetSavedConfirmedEventsBy
                    , setSavedConfirmedEvFor = SetSavedConfirmedEventsFor
                    , setAvailMap = SetAvailabilityMap
                    }
                )
                timeZoneLabel

        SetTimeSlotsElement result ->
            TSUpdate.setTimeSlotsElement model result

        SetAvailableTimesCount result ->
            TSUpdate.setAvailableTimesCount model result

        SetAvailabilityMap result ->
            TSUpdate.setAvailabilityMap model result

        StartSelectingTimeSlot dayNum slotNum ->
            TSUpdate.startSelectingTimeSlot model dayNum slotNum

        HandleTimeSlotMouseMove pointerPosition ->
            TSUpdate.handleTimeSlotMouseMove model AdjustTimeSlotSelection pointerPosition

        AdjustTimeSlotSelection pointerPosition result ->
            TSUpdate.adjustTimeSlotSelection model pointerPosition result

        HandleTimeSlotMouseUp ->
            TSUpdate.handleTimeSlotMouseUp model (CreateEvent {}) PromptUserForEventDetails

        EditTimeSlotSelection selectedTimeslotDetails ->
            TSUpdate.editTimeSlotSelection model PromptUserForEventDetails selectedTimeslotDetails

        SaveAvailableTimeSlot ->
            TSUpdate.saveAvailableTimeSlot model

        -- EventCreation
        PromptUserForEventDetails eventDetails result ->
            ECUpdate.promptUserForEventDetails model eventDetails result

        ChangeSelectionDayNum dayNum ->
            ECUpdate.changeSelectionDayNum model PromptUserForEventDetails dayNum

        ChangeSelectionStartSlot startSlot ->
            ECUpdate.changeSelectionStartSlot model PromptUserForEventDetails startSlot

        ChangeSelectionEndSlot endSlot ->
            ECUpdate.changeSelectionEndSlot model endSlot

        HandleEditingCancel ->
            ECUpdate.handleEditingCancel model

        CloseUserPromptForEventDetails ->
            ECUpdate.closeUserPromptForEventDetails model

        CancelDiscardConfirmationModal ->
            ECUpdate.cancelDiscardConfirmationModal model

        SaveEditingTimeSlotWithoutChanges ->
            ECUpdate.saveEditingTimeSlotWithoutChanges model



-- VIEW


view : Model -> Document Msg
view model =
    let
        docTitle =
            case model.proposedEvent of
                Just { title } ->
                    "Set the Meeting Time for - " ++ title

                Nothing ->
                    "Set the Meeting Time"
    in
    { title = docTitle
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
                    (CreateEvent
                        { onMdc = Mdc
                        , onTimeZoneSelect = UpdateTimeZone
                        , moveWeekBackward = MoveWeekBackward
                        , moveWeekForward = MoveWeekForward
                        }
                    )
                , viewDayHeadings model
                , viewScrollableTimeSlots model
                    (CreateEvent
                        { editTimeSlotSelection = EditTimeSlotSelection
                        , handleTimeSlotMouseMove = HandleTimeSlotMouseMove
                        , handleTimeSlotMouseUp = HandleTimeSlotMouseUp
                        , startSelectingTimeSlot = StartSelectingTimeSlot
                        }
                    )
                ]
            , viewDiscardConfirmationModal model
                { cancelDiscardConfirmationModal = CancelDiscardConfirmationModal
                , onMdc = Mdc
                , saveEditingTimeSlotWithoutChanges = SaveEditingTimeSlotWithoutChanges
                }
            ]
        , viewUserRequest model
            (CreateEvent
                { changeSelectionDayNum = ChangeSelectionDayNum
                , changeSelectionStartSlot = ChangeSelectionStartSlot
                , changeSelectionEndSlot = ChangeSelectionEndSlot
                , onMdc = Mdc
                , closeUserPromptForEventDetails = CloseUserPromptForEventDetails
                , saveTimeSlot = SaveAvailableTimeSlot
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
