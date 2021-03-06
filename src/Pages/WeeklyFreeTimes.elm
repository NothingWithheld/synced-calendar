module Pages.WeeklyFreeTimes exposing (Model, Msg, init, subscriptions, update, view)

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
import TimeSlots.Commands exposing (requestTimeSlotPositions, requestTimeSlotsElement)
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
    , alreadySubmittedConfirmedEvent : Bool
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , timeDetails = Nothing
      , loadingWeeklyFreeTimes = True
      , loadingConfirmedEventsBy = False
      , loadingConfirmedEventsFor = False
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
        ]
    )



-- UPDATE


type Msg
    = NoOp
      -- TimeSlots
    | SetTimeSlotPositions (Result Dom.Error (List Dom.Element))
    | UpdateTimeZone String
    | SetTimeSlotsElement (Result Dom.Error Dom.Element)
    | SetSavedWeeklyTimeSlots (Result Http.Error (List TSMessaging.ServerTimeSlot))
    | StartSelectingTimeSlot TS.DayNum TS.SlotNum
    | HandleTimeSlotMouseMove TS.PointerPosition
    | AdjustTimeSlotSelection TS.PointerPosition (Result Dom.Error Dom.Viewport)
    | SendSaveTimeSlotRequest
    | SendUpdateTimeSlotRequest
    | SetSelectedTimeSlotAfterCreation (Result Http.Error Int)
    | SetSelectedTimeSlotAfterEditing (Result Http.Error NoData)
    | HandleTimeSlotMouseUp
    | EditTimeSlotSelection TS.SelectedTimeSlotDetails
    | SendDeleteTimeSlotRequest
    | DeleteTimeSlot (Result Http.Error NoData)
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
        SetTimeSlotPositions result ->
            TSUpdate.setTimeSlotPositions model (WeeklyFreeTimes SetSavedWeeklyTimeSlots) result

        UpdateTimeZone timeZoneLabel ->
            TSUpdate.updateTimeZone model (WeeklyFreeTimes SetSavedWeeklyTimeSlots) timeZoneLabel

        SetTimeSlotsElement result ->
            TSUpdate.setTimeSlotsElement model result

        SetSavedWeeklyTimeSlots result ->
            TSUpdate.setSavedWeeklyTimeSlots model (WeeklyFreeTimes {}) result

        StartSelectingTimeSlot dayNum slotNum ->
            TSUpdate.startSelectingTimeSlot model dayNum slotNum

        HandleTimeSlotMouseMove pointerPosition ->
            TSUpdate.handleTimeSlotMouseMove model AdjustTimeSlotSelection pointerPosition

        AdjustTimeSlotSelection pointerPosition result ->
            TSUpdate.adjustTimeSlotSelection model pointerPosition result

        SendSaveTimeSlotRequest ->
            TSUpdate.sendSaveTimeSlotRequest model SetSelectedTimeSlotAfterCreation

        SendUpdateTimeSlotRequest ->
            TSUpdate.sendUpdateTimeSlotRequest model SetSelectedTimeSlotAfterEditing

        SetSelectedTimeSlotAfterCreation result ->
            TSUpdate.setSelectedTimeSlotAfterCreation model result

        SetSelectedTimeSlotAfterEditing result ->
            TSUpdate.setSelectedTimeSlotAfterEditing model result

        HandleTimeSlotMouseUp ->
            TSUpdate.handleTimeSlotMouseUp model (WeeklyFreeTimes {}) PromptUserForEventDetails

        EditTimeSlotSelection selectedTimeslotDetails ->
            TSUpdate.editTimeSlotSelection model PromptUserForEventDetails selectedTimeslotDetails

        SendDeleteTimeSlotRequest ->
            TSUpdate.sendDeleteTimeSlotRequest model DeleteTimeSlot

        DeleteTimeSlot result ->
            TSUpdate.deleteTimeSlot model result

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
                    (WeeklyFreeTimes
                        { onMdc = Mdc
                        , onTimeZoneSelect = UpdateTimeZone
                        }
                    )
                , viewDayHeadings model
                , viewScrollableTimeSlots model
                    (WeeklyFreeTimes
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
            (WeeklyFreeTimes
                { changeSelectionDayNum = ChangeSelectionDayNum
                , changeSelectionStartSlot = ChangeSelectionStartSlot
                , changeSelectionEndSlot = ChangeSelectionEndSlot
                , onMdc = Mdc
                , closeUserPromptForEventDetails = CloseUserPromptForEventDetails
                , saveTimeSlot = SendSaveTimeSlotRequest
                , deleteTimeSlot = SendDeleteTimeSlotRequest
                , handleEditingCancel = HandleEditingCancel
                , updateTimeSlot = SendUpdateTimeSlotRequest
                , noOp = NoOp
                }
            )
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model
