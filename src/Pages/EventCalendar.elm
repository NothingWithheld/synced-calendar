module Pages.EventCalendar exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Dom as Dom
import EventCreation.EventCreation as EC
import EventCreation.Update as ECUpdate
import EventCreation.View exposing (viewDiscardConfirmationModal, viewUserRequest)
import Html exposing (div)
import Http
import Material
import Material.Options exposing (css, styled)
import Session exposing (Session)
import Time exposing (Posix)
import TimeSlots.Commands
    exposing
        ( requestCurrentDay
        , requestTimeSlotPositions
        , requestTimeSlotsElement
        )
import TimeSlots.Time exposing (TimeDetails(..))
import TimeSlots.TimeSlots as TS exposing (Calendar(..))
import TimeSlots.Update as TSUpdate
import TimeSlots.View exposing (viewCalendarHeading, viewDayHeadings, viewScrollableTimeSlots)
import Utils exposing (NoData)



-- MODEL


type alias Model =
    { session : Session
    , timeDetails : TimeDetails
    , loadingTimeSlots : Bool
    , timeSlotPositions : List TS.TimeSlotBoundaryPosition
    , timeSlotsElement : Maybe TS.Element
    , timeSlotSelection : TS.TimeSlotSelection
    , eventCreation : EC.EventCreation
    , selectedTimeSlots : List TS.SelectedTimeSlotDetails
    , isDiscardConfirmationModalOpen : Bool
    , mdc : Material.Model Msg
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , timeDetails = WithoutTime
      , loadingTimeSlots = True
      , timeSlotPositions = []
      , timeSlotsElement = Nothing
      , timeSlotSelection = TS.NotSelecting
      , eventCreation = EC.NotCreating
      , selectedTimeSlots = []
      , isDiscardConfirmationModalOpen = False
      , mdc = Material.defaultModel
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
    | UpdateTimeZone String
    | SetTimeSlotsElement (Result Dom.Error Dom.Element)
    | SendSaveTimeSlotRequest
    | SendUpdateTimeSlotRequest
    | SetSelectedTimeSlotAfterCreation (Result Http.Error Int)
    | SetSelectedTimeSlotAfterEditing (Result Http.Error NoData)
    | SendDeleteTimeSlotRequest
    | DeleteTimeSlot (Result Http.Error NoData)
      -- EventCreation
    | PromptUserForEventDetails (Result Dom.Error Dom.Element)
    | AdjustEventTitle String
    | AdjustEventDescription String
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
            TSUpdate.setInitialTime model result

        MoveWeekForward ->
            TSUpdate.moveWeekForward model

        MoveWeekBackward ->
            TSUpdate.moveWeekBackward model

        SetTimeSlotPositions result ->
            TSUpdate.setTimeSlotPositions model (Events Nothing) result

        UpdateTimeZone timeZoneLabel ->
            TSUpdate.updateTimeZone model (Events Nothing) timeZoneLabel

        SetTimeSlotsElement result ->
            TSUpdate.setTimeSlotsElement model result

        SendSaveTimeSlotRequest ->
            TSUpdate.sendSaveTimeSlotRequest model SetSelectedTimeSlotAfterCreation

        SendUpdateTimeSlotRequest ->
            TSUpdate.sendUpdateTimeSlotRequest model SetSelectedTimeSlotAfterEditing

        SetSelectedTimeSlotAfterCreation result ->
            TSUpdate.setSelectedTimeSlotAfterCreation model result

        SetSelectedTimeSlotAfterEditing result ->
            TSUpdate.setSelectedTimeSlotAfterEditing model result

        SendDeleteTimeSlotRequest ->
            TSUpdate.sendDeleteTimeSlotRequest model DeleteTimeSlot

        DeleteTimeSlot result ->
            TSUpdate.deleteTimeSlot model result

        -- EventCreation
        PromptUserForEventDetails result ->
            ECUpdate.promptUserForEventDetails model result

        AdjustEventTitle title ->
            ECUpdate.adjustEventTitle model title

        AdjustEventDescription description ->
            ECUpdate.adjustEventDescription model description

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
                    (Events
                        { onMdc = Mdc
                        , onTimeZoneSelect = UpdateTimeZone
                        , moveWeekBackward = MoveWeekBackward
                        , moveWeekForward = MoveWeekForward
                        }
                    )
                , viewDayHeadings model
                , viewScrollableTimeSlots model (Events Nothing)
                ]
            , viewDiscardConfirmationModal model
                { cancelDiscardConfirmationModal = CancelDiscardConfirmationModal
                , onMdc = Mdc
                , saveEditingTimeSlotWithoutChanges = SaveEditingTimeSlotWithoutChanges
                }
            ]
        , viewUserRequest model
            { changeSelectionDayNum = ChangeSelectionDayNum
            , changeSelectionStartSlot = ChangeSelectionStartSlot
            , changeSelectionEndSlot = ChangeSelectionEndSlot
            , onMdc = Mdc
            , closeUserPromptForEventDetails = CloseUserPromptForEventDetails
            , sendSaveTimeSlotRequest = SendSaveTimeSlotRequest
            , sendDeleteTimeSlotRequest = SendDeleteTimeSlotRequest
            , handleEditingCancel = HandleEditingCancel
            , sendUpdateTimeSlotRequest = SendUpdateTimeSlotRequest
            , adjustEventTitle = AdjustEventTitle
            , adjustEventDescription = AdjustEventDescription
            , noOp = NoOp
            }
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model
