module EventCreation.View exposing (viewDiscardConfirmationModal, viewUserRequest)

import AvailableTime.AvailableTime as AT
import Date
import EventCreation.Constants as ECConsts
import EventCreation.EventCreation as EC
import Html exposing (Html, div, text)
import Html.Entity as Entity
import Json.Decode as Decode
import Material
import Material.Button as Button
import Material.Card as Card
import Material.IconButton as IconButton
import Material.Menu
import Material.Options as Options exposing (css, styled, when)
import Material.Select as Select
import Material.TextField as TextField
import Material.Typography as Typography
import ProposeEvent.ProposeEvent as PE
import Session exposing (WithSession)
import TimeSlots.Time as TSTime
import TimeSlots.TimeSlots as TS exposing (Calendar(..))
import Utils exposing (WithMdc, getListItemAt)



-- viewUserRequest


viewUserRequest :
    WithMdc msg (EC.WithEventCreation (TS.WithTimeSlotSelection (TS.WithSelectedTimeSlots (PE.WithProposedEvent (TSTime.WithTimeDetails (WithSession (AT.WithAvailabilityMap (AT.WithAvailableTimesCount a))))))))
    ->
        Calendar
            { b
                | changeSelectionDayNum : String -> msg
                , changeSelectionStartSlot : String -> msg
                , changeSelectionEndSlot : String -> msg
                , onMdc : Material.Msg msg -> msg
                , closeUserPromptForEventDetails : msg
                , saveTimeSlot : msg
                , deleteTimeSlot : msg
                , handleEditingCancel : msg
                , updateTimeSlot : msg
                , noOp : msg
            }
            { c
                | noOp : msg
                , handleEditingCancel : msg
                , onMdc : Material.Msg msg -> msg
            }
            { d
                | changeSelectionDayNum : String -> msg
                , changeSelectionStartSlot : String -> msg
                , changeSelectionEndSlot : String -> msg
                , onMdc : Material.Msg msg -> msg
                , closeUserPromptForEventDetails : msg
                , saveTimeSlot : msg
                , deleteTimeSlot : msg
                , handleEditingCancel : msg
                , updateTimeSlot : msg
                , noOp : msg
            }
            { e
                | noOp : msg
                , handleEditingCancel : msg
                , onMdc : Material.Msg msg -> msg
                , changeSelectionDayNum : String -> msg
                , changeSelectionStartSlot : String -> msg
                , changeSelectionEndSlot : String -> msg
                , closeUserPromptForEventDetails : msg
                , saveTimeSlot : msg
            }
    -> Html msg
viewUserRequest model updates =
    let
        handleEditingCancel =
            case updates of
                WeeklyFreeTimes updates_ ->
                    updates_.handleEditingCancel

                Events updates_ ->
                    updates_.handleEditingCancel

                SubmitAvailability updates_ ->
                    updates_.handleEditingCancel

                CreateEvent updates_ ->
                    updates_.handleEditingCancel

        maybeTimeSlot =
            case model.timeSlotSelection of
                TS.CurrentlySelecting timeSlot ->
                    Just <| TS.getOrderedTimeSlot timeSlot

                TS.EditingSelection timeSlot _ ->
                    Just timeSlot

                _ ->
                    Nothing
    in
    case ( model.eventCreation, maybeTimeSlot ) of
        ( EC.CurrentlyCreatingEvent eventCreationDetails eventCreationPosition, Just timeSlot ) ->
            styled div
                [ css "position" "absolute"
                , css "top" "0"
                , css "width" "100%"
                , css "height" "100%"
                , css "z-index" "100"
                , css "display" "flex"
                , css "flex-direction" "column"
                , Options.onClick handleEditingCancel
                ]
                [ styled div
                    [ css "max-height" <| String.fromFloat (max 0 eventCreationPosition.y) ++ "px"
                    , css "min-height" "32px"
                    , css "width" "0px"
                    , css "flex-grow" "1"
                    ]
                    []
                , styled div
                    [ css "display" "flex"
                    ]
                    [ styled div [ css "width" <| String.fromFloat eventCreationPosition.x ++ "px", css "height" "0px" ] []
                    , viewUserRequestForm model updates timeSlot eventCreationDetails
                    ]
                , styled div
                    [ css "min-height" "32px"
                    , css "width" "0px"
                    ]
                    []
                ]

        _ ->
            text ""


viewUserRequestForm :
    WithMdc msg (TS.WithTimeSlotSelection (TS.WithSelectedTimeSlots (PE.WithProposedEvent (TSTime.WithTimeDetails (WithSession (AT.WithAvailabilityMap (AT.WithAvailableTimesCount a)))))))
    ->
        Calendar
            { b
                | changeSelectionDayNum : String -> msg
                , changeSelectionStartSlot : String -> msg
                , changeSelectionEndSlot : String -> msg
                , onMdc : Material.Msg msg -> msg
                , closeUserPromptForEventDetails : msg
                , saveTimeSlot : msg
                , deleteTimeSlot : msg
                , handleEditingCancel : msg
                , updateTimeSlot : msg
                , noOp : msg
            }
            { c
                | noOp : msg
                , handleEditingCancel : msg
                , onMdc : Material.Msg msg -> msg
            }
            { d
                | changeSelectionDayNum : String -> msg
                , changeSelectionStartSlot : String -> msg
                , changeSelectionEndSlot : String -> msg
                , onMdc : Material.Msg msg -> msg
                , closeUserPromptForEventDetails : msg
                , saveTimeSlot : msg
                , deleteTimeSlot : msg
                , handleEditingCancel : msg
                , updateTimeSlot : msg
                , noOp : msg
            }
            { e
                | noOp : msg
                , handleEditingCancel : msg
                , onMdc : Material.Msg msg -> msg
                , changeSelectionDayNum : String -> msg
                , changeSelectionStartSlot : String -> msg
                , changeSelectionEndSlot : String -> msg
                , closeUserPromptForEventDetails : msg
                , saveTimeSlot : msg
            }
    -> TS.TimeSlot
    -> EC.EventDetails
    -> Html msg
viewUserRequestForm model updates timeSlot eventDetails =
    let
        selectedTimeSlotsThatWeek =
            TSTime.getSelectedTimeSlotsInThatWeek model

        intersectsTimeSlots =
            TS.doesTSSelectionIntersectSelectedTimeSlots
                selectedTimeSlotsThatWeek
                model.timeSlotSelection

        invalidSelection =
            case ( eventDetails, model.proposedEvent ) of
                ( EC.AvailableTime date, Just { fromDate, toDate } ) ->
                    (not <| Date.isBetween fromDate toDate date)
                        || intersectsTimeSlots

                ( EC.AvailableTime _, Nothing ) ->
                    True

                ( EC.UnsetConfirmedEvent { date }, Just { fromDate, toDate } ) ->
                    let
                        isInEventBounds =
                            Date.isBetween fromDate toDate date

                        isAvailable =
                            (model.countSubmitted == model.totalRecipients)
                                && (List.all
                                        (AT.isSlotAvailable model date)
                                    <|
                                        List.range
                                            timeSlot.startBound.slotNum
                                            timeSlot.endBound.slotNum
                                   )
                    in
                    not isInEventBounds || not isAvailable || intersectsTimeSlots

                ( EC.UnsetConfirmedEvent _, Nothing ) ->
                    True

                _ ->
                    intersectsTimeSlots

        noOp =
            case updates of
                WeeklyFreeTimes updates_ ->
                    updates_.noOp

                Events updates_ ->
                    updates_.noOp

                SubmitAvailability updates_ ->
                    updates_.noOp

                CreateEvent updates_ ->
                    updates_.noOp
    in
    Card.view
        [ when invalidSelection <| css "border" "2px solid #D64545"
        , css "width" (String.fromFloat EC.eventDetailsPromptWidth ++ "px")
        , css "padding" "12px 8px 0px"
        , css "box-shadow" "0 24px 38px 3px rgba(0,0,0,0.14), 0 9px 46px 8px rgba(0,0,0,0.12), 0 11px 15px -7px rgba(0,0,0,0.2)"
        , Options.onWithOptions "click"
            (Decode.succeed
                { message = noOp
                , preventDefault = False
                , stopPropagation = True
                }
            )
        ]
        (case ( eventDetails, updates ) of
            ( EC.UnsetWeeklyFreeTime, WeeklyFreeTimes updates_ ) ->
                viewChangeTimeSlotForm model updates_ invalidSelection

            ( EC.SetWeeklyFreeTime _, WeeklyFreeTimes updates_ ) ->
                viewChangeTimeSlotForm model updates_ invalidSelection

            ( EC.AvailableTime _, SubmitAvailability udpates_ ) ->
                viewChangeTimeSlotForm model udpates_ invalidSelection

            ( EC.ConfirmedEvent confirmedEventDetails, SubmitAvailability udpates_ ) ->
                viewConfirmedEventForm model udpates_ timeSlot confirmedEventDetails

            ( EC.UnsetConfirmedEvent confirmedEventDetails, Events udpates_ ) ->
                viewConfirmedEventForm model udpates_ timeSlot confirmedEventDetails

            ( EC.ConfirmedEvent confirmedEventDetails, Events udpates_ ) ->
                viewConfirmedEventForm model udpates_ timeSlot confirmedEventDetails

            ( EC.UnsetConfirmedEvent confirmedEventDetails, CreateEvent udpates_ ) ->
                viewUnsetConfirmedEventForm model udpates_ confirmedEventDetails invalidSelection

            ( EC.ConfirmedEvent confirmedEventDetails, CreateEvent udpates_ ) ->
                viewConfirmedEventForm model udpates_ timeSlot confirmedEventDetails

            _ ->
                []
        )


viewChangeTimeSlotForm :
    WithMdc msg (TS.WithTimeSlotSelection a)
    ->
        { b
            | changeSelectionDayNum : String -> msg
            , changeSelectionStartSlot : String -> msg
            , changeSelectionEndSlot : String -> msg
            , onMdc : Material.Msg msg -> msg
            , closeUserPromptForEventDetails : msg
            , saveTimeSlot : msg
            , deleteTimeSlot : msg
            , handleEditingCancel : msg
            , updateTimeSlot : msg
        }
    -> Bool
    -> List (Html msg)
viewChangeTimeSlotForm model updates invalidSelection =
    case model.timeSlotSelection of
        TS.CurrentlySelecting _ ->
            [ viewTimeChangeSelects model updates
            , viewInitialCreationActionButtons model updates invalidSelection
            ]

        TS.EditingSelection _ _ ->
            [ viewTimeChangeSelects model updates
            , viewEditingActionButtons model updates invalidSelection
            ]

        _ ->
            []


viewUnsetConfirmedEventForm :
    WithMdc msg (TS.WithTimeSlotSelection a)
    ->
        { b
            | onMdc : Material.Msg msg -> msg
            , changeSelectionDayNum : String -> msg
            , changeSelectionStartSlot : String -> msg
            , changeSelectionEndSlot : String -> msg
            , closeUserPromptForEventDetails : msg
            , saveTimeSlot : msg
        }
    -> EC.ConfirmedEventDetails
    -> Bool
    -> List (Html msg)
viewUnsetConfirmedEventForm model updates confirmedEventDetails invalidSelection =
    [ styled Html.h4
        [ Typography.headline5
        , css "margin" "0 0 4px 4px"
        ]
        [ text confirmedEventDetails.title
        ]
    , if String.length confirmedEventDetails.description > 0 then
        styled Html.p [ css "margin" "4px 0 4px 4px" ] [ text confirmedEventDetails.description ]

      else
        text ""
    , viewTimeChangeSelects model updates
    , viewInitialCreationActionButtons model updates invalidSelection
    ]


viewConfirmedEventForm :
    WithMdc msg a
    ->
        { b
            | handleEditingCancel : msg
            , onMdc : Material.Msg msg -> msg
        }
    -> TS.TimeSlot
    -> EC.ConfirmedEventDetails
    -> List (Html msg)
viewConfirmedEventForm model { onMdc, handleEditingCancel } { startBound, endBound } confirmedEventDetails =
    let
        ( startTime, startAmOrPm ) =
            TS.getTimeForSlotNum startBound.slotNum False

        ( endTime, endAmOrPm ) =
            TS.getTimeForSlotNum endBound.slotNum True

        startsAndEndsSameHalfOfDay =
            startAmOrPm == endAmOrPm
    in
    [ styled Html.h4
        [ Typography.headline5
        , css "margin" "0 0 8px 4px"
        ]
        [ text confirmedEventDetails.title
        ]
    , styled div
        [ css "display" "flex"
        , css "align-items" "center"
        , css "margin-left" "4px"
        ]
        [ styled Html.p
            [ css "margin" "0" ]
            [ text <| Date.format "MMMM d, y" confirmedEventDetails.date ]
        , styled Html.p
            [ css "margin" "0 0 0 24px" ]
            [ text <|
                startTime
                    ++ (if startsAndEndsSameHalfOfDay then
                            ""

                        else
                            startAmOrPm
                       )
                    ++ " "
                    ++ Entity.ndash
                    ++ " "
                    ++ endTime
                    ++ endAmOrPm
            ]
        ]
    , styled Html.p [ css "margin" "4px 0 4px 4px" ] [ text confirmedEventDetails.description ]
    , Card.actions [ css "display" "flex", css "flex-direction" "row-reverse" ]
        [ Card.actionButtons []
            [ Button.view onMdc
                "close-event-button"
                model.mdc
                [ Card.actionButton
                , Button.ripple
                , Button.unelevated
                , Options.onClick handleEditingCancel
                ]
                [ text "Cancel" ]
            ]
        ]
    ]


viewEventDetailsForm :
    WithMdc msg (TS.WithTimeSlotSelection a)
    ->
        { b
            | closeUserPromptForEventDetails : msg
            , saveTimeSlot : msg
            , onMdc : Material.Msg msg -> msg
            , changeSelectionDayNum : String -> msg
            , changeSelectionStartSlot : String -> msg
            , changeSelectionEndSlot : String -> msg
            , adjustEventTitle : String -> msg
            , adjustEventDescription : String -> msg
        }
    -> EC.ConfirmedEventDetails
    -> Bool
    -> List (Html msg)
viewEventDetailsForm model ({ onMdc, adjustEventTitle, adjustEventDescription } as updates) eventItems invalidSelection =
    [ TextField.view onMdc
        "event-title"
        model.mdc
        [ TextField.label "Title"
        , TextField.value eventItems.title
        , Options.onInput adjustEventTitle
        ]
        []
    , viewTimeChangeSelects model updates
    , TextField.view onMdc
        "event-description"
        model.mdc
        [ TextField.label "Description"
        , TextField.value eventItems.description
        , Options.onInput adjustEventDescription
        ]
        []
    , viewInitialCreationActionButtons model updates invalidSelection
    ]


viewInitialCreationActionButtons :
    WithMdc msg a
    ->
        { b
            | closeUserPromptForEventDetails : msg
            , saveTimeSlot : msg
            , onMdc : Material.Msg msg -> msg
        }
    -> Bool
    -> Html msg
viewInitialCreationActionButtons model { closeUserPromptForEventDetails, saveTimeSlot, onMdc } invalidSelection =
    Card.actions [ css "display" "flex", css "flex-direction" "row-reverse" ]
        [ Card.actionButtons []
            [ Button.view onMdc
                "close-event-button"
                model.mdc
                [ Card.actionButton
                , Button.ripple
                , Options.onClick closeUserPromptForEventDetails
                , css "margin-right" "8px"
                ]
                [ text "Cancel" ]
            , Button.view onMdc
                "set-event-button"
                model.mdc
                [ Card.actionButton
                , Button.ripple
                , Button.unelevated
                , Options.onClick saveTimeSlot
                , when invalidSelection Button.disabled
                ]
                [ text "Submit" ]
            ]
        ]


viewEditingActionButtons :
    WithMdc msg a
    ->
        { b
            | deleteTimeSlot : msg
            , handleEditingCancel : msg
            , updateTimeSlot : msg
            , onMdc : Material.Msg msg -> msg
        }
    -> Bool
    -> Html msg
viewEditingActionButtons model updates invalidSelection =
    let
        { deleteTimeSlot, handleEditingCancel, updateTimeSlot, onMdc } =
            updates
    in
    Card.actions [ css "display" "flex", css "flex-direction" "row-reverse" ]
        [ Card.actionButtons []
            [ IconButton.view onMdc
                "trash-event-button"
                model.mdc
                [ IconButton.icon1 "delete"
                , IconButton.label1 "Delete this time slot"
                , Options.onClick deleteTimeSlot
                ]
                []
            , Button.view onMdc
                "cancel-event-button"
                model.mdc
                [ Card.actionButton
                , Button.ripple
                , Options.onClick handleEditingCancel
                , css "margin-right" "8px"
                ]
                [ text "Cancel" ]
            , Button.view onMdc
                "set-event-button"
                model.mdc
                [ Card.actionButton
                , Button.ripple
                , Button.unelevated
                , Options.onClick updateTimeSlot
                , when invalidSelection Button.disabled
                ]
                [ text "Submit" ]
            ]
        ]


viewTimeChangeSelects :
    WithMdc msg (TS.WithTimeSlotSelection a)
    ->
        { b
            | changeSelectionDayNum : String -> msg
            , changeSelectionStartSlot : String -> msg
            , changeSelectionEndSlot : String -> msg
            , onMdc : Material.Msg msg -> msg
        }
    -> Html msg
viewTimeChangeSelects model updates =
    case model.timeSlotSelection of
        TS.CurrentlySelecting selectedTimeSlot ->
            viewWeeklyFreeTimesSelects model updates selectedTimeSlot

        TS.EditingSelection selectedTimeSlot _ ->
            viewWeeklyFreeTimesSelects model updates selectedTimeSlot

        _ ->
            text ""


viewWeeklyFreeTimesSelects :
    WithMdc msg a
    ->
        { b
            | changeSelectionDayNum : String -> msg
            , changeSelectionStartSlot : String -> msg
            , changeSelectionEndSlot : String -> msg
            , onMdc : Material.Msg msg -> msg
        }
    -> TS.TimeSlot
    -> Html msg
viewWeeklyFreeTimesSelects model updates { dayNum, startBound, endBound } =
    styled div
        [ css "display" "flex"
        , css "justify-content" "space-between"
        ]
        [ viewDayChangeSelect model
            updates
            dayNum
        , viewStartTimeSlotSelect model
            updates
            startBound.slotNum
        , viewEndTimeSlotSelect model
            updates
            startBound.slotNum
            endBound.slotNum
        ]


viewDayChangeSelect :
    WithMdc msg a
    ->
        { b
            | changeSelectionDayNum : String -> msg
            , onMdc : Material.Msg msg -> msg
        }
    -> TS.DayNum
    -> Html msg
viewDayChangeSelect model { changeSelectionDayNum, onMdc } selectedDayNum =
    let
        maybeSelectedDayAbbr =
            getListItemAt selectedDayNum TS.dayAbbreviations

        selectedDayAbbr =
            Maybe.withDefault "" maybeSelectedDayAbbr
    in
    Select.view onMdc
        ECConsts.daySelectId
        model.mdc
        [ Select.label "Day"
        , Select.selectedText selectedDayAbbr
        , Select.required
        , Select.onSelect changeSelectionDayNum
        ]
    <|
        List.map
            (\( dayNum, label ) ->
                viewDaySelectOption dayNum label <| dayNum == selectedDayNum
            )
        <|
            List.map2
                Tuple.pair
                TS.dayNumRange
                TS.dayAbbreviations


viewDaySelectOption : TS.DayNum -> String -> Bool -> Material.Menu.Item msg
viewDaySelectOption dayNum label isSelected =
    Select.option
        [ Select.value <| String.fromInt dayNum
        , when isSelected
            Select.selected
        ]
        [ text <| label ]


viewStartTimeSlotSelect :
    WithMdc msg a
    ->
        { b
            | changeSelectionStartSlot : String -> msg
            , onMdc : Material.Msg msg -> msg
        }
    -> TS.SlotNum
    -> Html msg
viewStartTimeSlotSelect model { changeSelectionStartSlot, onMdc } selectedSlotNum =
    let
        ( startTime, startAmOrPm ) =
            TS.getTimeForSlotNum selectedSlotNum False
    in
    Select.view onMdc
        ECConsts.startTimeSelectId
        model.mdc
        [ Select.label "Start"
        , Select.selectedText <| startTime ++ startAmOrPm
        , Select.required
        , Select.onSelect changeSelectionStartSlot
        ]
    <|
        List.map
            (\slotNum ->
                viewTimeSlotSelectOption "" False slotNum <| slotNum == selectedSlotNum
            )
            TS.slotNumRange


viewEndTimeSlotSelect :
    WithMdc msg a
    ->
        { b
            | changeSelectionEndSlot : String -> msg
            , onMdc : Material.Msg msg -> msg
        }
    -> TS.SlotNum
    -> TS.SlotNum
    -> Html msg
viewEndTimeSlotSelect model { changeSelectionEndSlot, onMdc } selectedStartSlotNum selectedEndSlotNum =
    let
        ( endTime, endAmOrPm ) =
            TS.getTimeForSlotNum selectedEndSlotNum True
    in
    Select.view onMdc
        ECConsts.endTimeSelectId
        model.mdc
        [ Select.label "End"
        , Select.selectedText <| endTime ++ endAmOrPm
        , Select.required
        , Select.onSelect changeSelectionEndSlot
        ]
    <|
        List.map
            (\slotNum ->
                viewTimeSlotSelectOption
                    (" (" ++ TS.getTimeDurationBetween selectedStartSlotNum slotNum ++ ")")
                    True
                    slotNum
                <|
                    slotNum
                        == selectedEndSlotNum
            )
        <|
            List.filter ((<=) selectedStartSlotNum) TS.slotNumRange


viewTimeSlotSelectOption :
    String
    -> Bool
    -> TS.SlotNum
    -> Bool
    -> Material.Menu.Item msg
viewTimeSlotSelectOption labelAddition isEndSlot slotNum isSelected =
    let
        ( time, amOrPm ) =
            TS.getTimeForSlotNum slotNum isEndSlot
    in
    Select.option
        [ Select.value <| String.fromInt slotNum
        , when isSelected
            Select.selected
        ]
        [ text <| time ++ amOrPm ++ labelAddition ]



-- viewDiscardConfirmationModal


viewDiscardConfirmationModal :
    WithMdc msg (EC.WithDiscardConfirmationModal a)
    ->
        { b
            | cancelDiscardConfirmationModal : msg
            , saveEditingTimeSlotWithoutChanges : msg
            , onMdc : Material.Msg msg -> msg
        }
    -> Html msg
viewDiscardConfirmationModal model { onMdc, cancelDiscardConfirmationModal, saveEditingTimeSlotWithoutChanges } =
    if model.isDiscardConfirmationModalOpen then
        styled div
            [ css "position" "absolute"
            , css "background-color" "rgba(0,0,0,0.4)"
            , css "z-index" "101"
            , css "height" "100%"
            , css "width" "100%"
            , css "display" "flex"
            , css "justify-content" "center"
            , css "align-items" "center"
            ]
            [ Card.view [ css "padding" "4px 12px 8px" ]
                [ styled div
                    []
                    [ styled Html.h2
                        [ Typography.headline6 ]
                        [ text "Discard unsaved changes?" ]
                    ]
                , Card.actions [ css "display" "flex", css "flex-direction" "row-reverse" ]
                    [ Card.actionButtons []
                        [ Button.view onMdc
                            "close-event-button"
                            model.mdc
                            [ Card.actionButton
                            , Button.ripple
                            , Options.onClick cancelDiscardConfirmationModal
                            , css "margin-right" "8px"
                            ]
                            [ text "Cancel" ]
                        , Button.view onMdc
                            "set-event-button"
                            model.mdc
                            [ Card.actionButton
                            , Button.ripple
                            , Button.unelevated
                            , Options.onClick saveEditingTimeSlotWithoutChanges
                            ]
                            [ text "Discard" ]
                        ]
                    ]
                ]
            ]

    else
        text ""
