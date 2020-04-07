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


viewUserRequest : WithMdc (EC.WithEventCreation (TS.WithTimeSlotSelection a)) -> Html Msg
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
                , Options.onClick CloseUserPromptForEventDetails
                ]
                [ viewUserRequestForm model eventCreationDetails eventCreationPosition ]


viewUserRequestForm : WithMdc (EC.WithEventCreation (TS.WithTimeSlotSelection a)) -> EC.EventCreationDetails -> EC.EventCreationPosition -> Html Msg
viewUserRequestForm model eventCreationDetails eventCreationPosition =
    let
        positionFromTop =
            Basics.max eventCreationPosition.y 50
    in
    Card.view
        [ css "position" "absolute"
        , css "width" (String.fromFloat EC.eventDetailsPromptWidth ++ "px")
        , css "left" (String.fromFloat eventCreationPosition.x ++ "px")
        , css "top" (String.fromFloat positionFromTop ++ "px")
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
                , viewTimeSlotSelect model startBound.slotNum False
                , viewTimeSlotSelect model endBound.slotNum True
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


viewTimeSlotSelect : WithMdc a -> TS.SlotNum -> Bool -> Html Msg
viewTimeSlotSelect model selectedSlotNum isEndSlot =
    let
        ( startTime, startAmOrPm ) =
            TS.getTimeForSlotNum selectedSlotNum isEndSlot
    in
    Select.view Mdc
        (if isEndSlot then
            ECConsts.endTimeSelectId

         else
            ECConsts.startTimeSelectId
        )
        model.mdc
        [ Select.label <|
            if isEndSlot then
                "End"

            else
                "Start"
        , Select.selectedText <| startTime ++ startAmOrPm
        , Select.required
        ]
    <|
        List.map
            (\slotNum ->
                viewTimeSlotSelectOption isEndSlot slotNum <| slotNum == selectedSlotNum
            )
            TS.slotNumRange


viewTimeSlotSelectOption : Bool -> TS.SlotNum -> Bool -> Material.Menu.Item Msg
viewTimeSlotSelectOption isEndSlot slotNum isSelected =
    let
        ( time, amOrPm ) =
            TS.getTimeForSlotNum slotNum isEndSlot
    in
    Select.option
        [ Select.value <| String.fromInt slotNum
        , when isSelected
            Select.selected
        ]
        [ text <| time ++ amOrPm ]
