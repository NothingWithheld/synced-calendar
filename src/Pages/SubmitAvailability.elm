module Pages.SubmitAvailability exposing (Model, Msg, init, subscriptions, update, view)

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
      , loadingWeeklyFreeTimes = True
      , loadingConfirmedEventsBy = True
      , loadingConfirmedEventsFor = True
      , loadingAvailableTimes = True
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
    | HandleSavedAvailableTimesForUser (Result Http.Error (List AvailableTimeDetails))
    | UpdateTimeZone String
    | SetTimeSlotsElement (Result Dom.Error Dom.Element)
    | SetSavedWeeklyTimeSlots (Result Http.Error (List TSMessaging.ServerTimeSlot))
    | StartSelectingTimeSlot TS.DayNum TS.SlotNum
    | HandleTimeSlotMouseMove TS.PointerPosition
    | AdjustTimeSlotSelection TS.PointerPosition (Result Dom.Error Dom.Viewport)
    | HandleTimeSlotMouseUp
    | EditTimeSlotSelection TS.SelectedTimeSlotDetails
    | SaveAvailableTimeSlot
    | SubmitAvailableTimes
    | AcknowledgeAvailableTimesSubmission (Result Http.Error NoData)
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
            TSUpdate.setInitialTime model (SubmitAvailability {}) result

        MoveWeekForward ->
            TSUpdate.moveWeekForward model

        MoveWeekBackward ->
            TSUpdate.moveWeekBackward model

        SetTimeSlotPositions result ->
            TSUpdate.setTimeSlotPositions model
                (SubmitAvailability
                    { setSavedConfirmedEvBy = SetSavedConfirmedEventsBy
                    , setSavedConfirmedEvFor = SetSavedConfirmedEventsFor
                    , setSavedWeeklyTS = SetSavedWeeklyTimeSlots
                    }
                )
                result

        SetSavedConfirmedEventsBy result ->
            TSUpdate.setSavedConfirmedEventsBy model
                (SubmitAvailability { handleSavedATForUser = HandleSavedAvailableTimesForUser })
                result

        SetSavedConfirmedEventsFor result ->
            TSUpdate.setSavedConfirmedEventsFor model
                (SubmitAvailability { handleSavedATForUser = HandleSavedAvailableTimesForUser })
                result

        HandleSavedAvailableTimesForUser result ->
            TSUpdate.handleSavedAvailableTimesForUser model result

        UpdateTimeZone timeZoneLabel ->
            TSUpdate.updateTimeZone model
                (SubmitAvailability
                    { setSavedConfirmedEvBy = SetSavedConfirmedEventsBy
                    , setSavedConfirmedEvFor = SetSavedConfirmedEventsFor
                    , setSavedWeeklyTS = SetSavedWeeklyTimeSlots
                    }
                )
                timeZoneLabel

        SetTimeSlotsElement result ->
            TSUpdate.setTimeSlotsElement model result

        SetSavedWeeklyTimeSlots result ->
            TSUpdate.setSavedWeeklyTimeSlots model
                (SubmitAvailability { handleSavedATForUser = HandleSavedAvailableTimesForUser })
                result

        StartSelectingTimeSlot dayNum slotNum ->
            TSUpdate.startSelectingTimeSlot model dayNum slotNum

        HandleTimeSlotMouseMove pointerPosition ->
            TSUpdate.handleTimeSlotMouseMove model AdjustTimeSlotSelection pointerPosition

        AdjustTimeSlotSelection pointerPosition result ->
            TSUpdate.adjustTimeSlotSelection model pointerPosition result

        HandleTimeSlotMouseUp ->
            TSUpdate.handleTimeSlotMouseUp model (SubmitAvailability {}) PromptUserForEventDetails

        EditTimeSlotSelection selectedTimeslotDetails ->
            TSUpdate.editTimeSlotSelection model PromptUserForEventDetails selectedTimeslotDetails

        SaveAvailableTimeSlot ->
            TSUpdate.saveAvailableTimeSlot model

        SubmitAvailableTimes ->
            TSUpdate.submitAvailableTimes model AcknowledgeAvailableTimesSubmission

        AcknowledgeAvailableTimesSubmission result ->
            TSUpdate.acknowledgeAvailableTimesSubmission model result

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
                    "Submit When You're Available for - " ++ title

                Nothing ->
                    "Submit When You're Available"
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
                    (SubmitAvailability
                        { onMdc = Mdc
                        , onTimeZoneSelect = UpdateTimeZone
                        , moveWeekBackward = MoveWeekBackward
                        , moveWeekForward = MoveWeekForward
                        , submitAvailability = SubmitAvailableTimes
                        }
                    )
                , viewDayHeadings model
                , viewScrollableTimeSlots model
                    (SubmitAvailability
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
            (SubmitAvailability
                { changeSelectionDayNum = ChangeSelectionDayNum
                , changeSelectionStartSlot = ChangeSelectionStartSlot
                , changeSelectionEndSlot = ChangeSelectionEndSlot
                , onMdc = Mdc
                , closeUserPromptForEventDetails = CloseUserPromptForEventDetails
                , saveTimeSlot = SaveAvailableTimeSlot
                , deleteTimeSlot = CloseUserPromptForEventDetails
                , handleEditingCancel = HandleEditingCancel
                , updateTimeSlot = SaveAvailableTimeSlot
                , noOp = NoOp
                }
            )
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model
