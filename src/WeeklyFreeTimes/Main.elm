module WeeklyFreeTimes.Main exposing (Model, init, subscriptions, update, view)

import Browser exposing (Document)
import EventCreation.EventCreation as EC
import EventCreation.Update as ECUpdate
import EventCreation.View exposing (viewDiscardConfirmationModal, viewUserRequest)
import Html exposing (div)
import Material
import Material.Options exposing (css, styled)
import TimeSlots.Commands exposing (requestTimeSlotPositions, requestTimeSlotsElement)
import TimeSlots.TimeSlots as TS
import TimeSlots.Update as TSUpdate
import TimeSlots.View exposing (viewDayHeadings, viewScrollableTimeSlots)
import WeeklyFreeTimes.MainMsg exposing (Msg(..))



-- MODEL


type alias Model =
    { userId : String
    , loadingTimeSlots : Bool
    , timeSlotPositions : List TS.TimeSlotBoundaryPosition
    , timeSlotsElement : Maybe TS.Element
    , timeSlotSelection : TS.TimeSlotSelection
    , eventCreation : EC.EventCreation
    , selectedTimeSlots : List TS.SelectedTimeSlotDetails
    , isDiscardConfirmationModalOpen : Bool
    , mdc : Material.Model Msg
    }


init : ( Model, Cmd Msg )
init =
    ( { userId = "25"
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
        , requestTimeSlotPositions
        , requestTimeSlotsElement
        ]
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Mdc msg_ ->
            Material.update Mdc msg_ model

        -- TimeSlots
        SetTimeSlotPositions result ->
            TSUpdate.setTimeSlotPositions model result

        SetTimeSlotsElement result ->
            TSUpdate.setTimeSlotsElement model result

        SetSavedWeeklyTimeSlots result ->
            TSUpdate.setSavedWeeklyTimeSlots model result

        StartSelectingTimeSlot dayNum slotNum ->
            TSUpdate.startSelectingTimeSlot model dayNum slotNum

        HandleTimeSlotMouseMove pointerPosition ->
            TSUpdate.handleTimeSlotMouseMove model pointerPosition

        AdjustTimeSlotSelection pointerPosition result ->
            TSUpdate.adjustTimeSlotSelection model pointerPosition result

        SendSaveTimeSlotRequest ->
            TSUpdate.sendSaveTimeSlotRequest model

        SendUpdateTimeSlotRequest ->
            TSUpdate.sendUpdateTimeSlotRequest model

        SetSelectedTimeSlotAfterCreation result ->
            TSUpdate.setSelectedTimeSlotAfterCreation model result

        SetSelectedTimeSlotAfterEditing result ->
            TSUpdate.setSelectedTimeSlotAfterEditing model result

        HandleTimeSlotMouseUp ->
            TSUpdate.handleTimeSlotMouseUp model

        EditTimeSlotSelection selectedTimeslotDetails ->
            TSUpdate.editTimeSlotSelection model selectedTimeslotDetails

        SendDeleteTimeSlotRequest ->
            TSUpdate.sendDeleteTimeSlotRequest model

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
            ECUpdate.changeSelectionDayNum model dayNum

        ChangeSelectionStartSlot startSlot ->
            ECUpdate.changeSelectionStartSlot model startSlot

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
                [ viewDayHeadings
                , viewScrollableTimeSlots model
                ]
            ]
        , viewUserRequest model
        , viewDiscardConfirmationModal model
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model
