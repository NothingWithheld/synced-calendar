module EventCreation.View exposing (viewUserRequest)

import EventCreation.Constants as ECConsts
import EventCreation.EventCreation as EC
import Html exposing (Html, div, text)
import Json.Decode as Decode
import MainMsg exposing (Msg(..), WithMdc)
import Material.Button as Button
import Material.Card as Card
import Material.Menu
import Material.Options as Options exposing (css, styled, when)
import Material.Select as Select
import Material.TextField as TextField
import TimeSlots.TimeSlots as TS
import Utils exposing (getListItemAt)


viewUserRequest : WithMdc (EC.WithEventCreation (TS.WithTimeSlotSelection (TS.WithSelectedTimeSlots a))) -> Html Msg
viewUserRequest model =
    case model.eventCreation of
        EC.NotCreating ->
            text ""

        EC.CurrentlyCreatingEvent eventCreationDetails eventCreationPosition ->
            styled div
                [ css "position" "absolute"
                , css "top" "0"
                , css "width" "100%"
                , css "height" "100%"
                , css "z-index" "100"
                , css "display" "flex"
                , css "flex-direction" "column"
                , Options.onClick CloseUserPromptForEventDetails
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
                    , viewUserRequestForm model eventCreationDetails
                    ]
                , styled div
                    [ css "min-height" "32px"
                    , css "width" "0px"
                    ]
                    []
                ]


viewUserRequestForm :
    WithMdc (TS.WithTimeSlotSelection (TS.WithSelectedTimeSlots a))
    -> EC.EventCreationDetails
    -> Html Msg
viewUserRequestForm model eventCreationDetails =
    let
        intersectsTimeSlots =
            TS.doesTSSelectionIntersectSelectedTimeSlots
                model.selectedTimeSlots
                model.timeSlotSelection
    in
    Card.view
        [ when intersectsTimeSlots <| css "border" "2px solid #D64545"
        , css "width" (String.fromFloat EC.eventDetailsPromptWidth ++ "px")
        , css "padding" "12px 8px 0px"
        , css "box-shadow" "0 24px 38px 3px rgba(0,0,0,0.14), 0 9px 46px 8px rgba(0,0,0,0.12), 0 11px 15px -7px rgba(0,0,0,0.2)"
        , Options.onWithOptions "click"
            (Decode.succeed
                { message = NoOp
                , preventDefault = False
                , stopPropagation = True
                }
            )
        ]
        [ TextField.view Mdc
            "event-title"
            model.mdc
            [ TextField.label "Title"
            , TextField.value eventCreationDetails.title
            , Options.onInput AdjustEventTitle
            ]
            []
        , TextField.view Mdc
            "event-description"
            model.mdc
            [ TextField.label "Description"
            , TextField.value eventCreationDetails.description
            , Options.onInput AdjustEventDescription
            ]
            []
        , viewTimeChangeSelects model
        , Card.actions [ css "display" "flex", css "flex-direction" "row-reverse" ]
            [ Card.actionButtons []
                [ Button.view Mdc
                    "close-event-button"
                    model.mdc
                    [ Card.actionButton
                    , Button.ripple
                    , Options.onClick CloseUserPromptForEventDetails
                    , css "margin-right" "8px"
                    ]
                    [ text "Cancel" ]
                , Button.view Mdc
                    "set-event-button"
                    model.mdc
                    [ Card.actionButton
                    , Button.ripple
                    , Button.unelevated
                    , Options.onClick SetSelectedTimeSlot
                    , when intersectsTimeSlots Button.disabled
                    ]
                    [ text "Submit" ]
                ]
            ]
        ]


viewTimeChangeSelects : WithMdc (TS.WithTimeSlotSelection a) -> Html Msg
viewTimeChangeSelects model =
    case model.timeSlotSelection of
        TS.CurrentlySelecting { dayNum, startBound, endBound } ->
            styled div
                [ css "display" "flex"
                , css "justify-content" "space-between"
                ]
                [ viewDayChangeSelect model dayNum
                , viewStartTimeSlotSelect model startBound.slotNum
                , viewEndTimeSlotSelect model startBound.slotNum endBound.slotNum
                ]

        _ ->
            text ""


viewDayChangeSelect : WithMdc a -> TS.DayNum -> Html Msg
viewDayChangeSelect model selectedDayNum =
    let
        maybeSelectedDayAbbr =
            getListItemAt selectedDayNum TS.dayAbbreviations

        selectedDayAbbr =
            Maybe.withDefault "" maybeSelectedDayAbbr
    in
    Select.view Mdc
        ECConsts.daySelectId
        model.mdc
        [ Select.label "Day"
        , Select.selectedText selectedDayAbbr
        , Select.required
        , Select.onSelect ChangeSelectionDayNum
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


viewDaySelectOption : TS.DayNum -> String -> Bool -> Material.Menu.Item Msg
viewDaySelectOption dayNum label isSelected =
    Select.option
        [ Select.value <| String.fromInt dayNum
        , when isSelected
            Select.selected
        ]
        [ text <| label ]


viewStartTimeSlotSelect : WithMdc a -> TS.SlotNum -> Html Msg
viewStartTimeSlotSelect model selectedSlotNum =
    let
        ( startTime, startAmOrPm ) =
            TS.getTimeForSlotNum selectedSlotNum False
    in
    Select.view Mdc
        ECConsts.startTimeSelectId
        model.mdc
        [ Select.label "Start"
        , Select.selectedText <| startTime ++ startAmOrPm
        , Select.required
        , Select.onSelect ChangeSelectionStartSlot
        ]
    <|
        List.map
            (\slotNum ->
                viewTimeSlotSelectOption "" False slotNum <| slotNum == selectedSlotNum
            )
            TS.slotNumRange


viewEndTimeSlotSelect : WithMdc a -> TS.SlotNum -> TS.SlotNum -> Html Msg
viewEndTimeSlotSelect model selectedStartSlotNum selectedEndSlotNum =
    let
        ( endTime, endAmOrPm ) =
            TS.getTimeForSlotNum selectedEndSlotNum True
    in
    Select.view Mdc
        ECConsts.endTimeSelectId
        model.mdc
        [ Select.label "End"
        , Select.selectedText <| endTime ++ endAmOrPm
        , Select.required
        , Select.onSelect ChangeSelectionEndSlot
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


viewTimeSlotSelectOption : String -> Bool -> TS.SlotNum -> Bool -> Material.Menu.Item Msg
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
