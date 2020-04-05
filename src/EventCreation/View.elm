module EventCreation.View exposing (viewUserRequest)

import EventCreation.EventCreation as EC
import Html exposing (Html, div, text)
import Json.Decode as Decode
import MainMsg exposing (Msg(..), WithMdc)
import Material.Button as Button
import Material.Card as Card
import Material.Options as Options exposing (css, styled)
import Material.TextField as TextField
import TimeSlots.TimeSlots as TS


viewUserRequest : WithMdc (EC.WithEventCreation a) -> Html Msg
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
                , Options.onClick <| EventCreationMsg EC.CloseUserPromptForEventDetails
                ]
                [ viewUserRequestForm model eventCreationDetails eventCreationPosition ]


viewUserRequestForm : WithMdc (EC.WithEventCreation a) -> EC.EventCreationDetails -> EC.EventCreationPosition -> Html Msg
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
            , Options.onInput <| EventCreationMsg << EC.AdjustEventTitle
            ]
            []
        , TextField.view Mdc
            "event-description"
            model.mdc
            [ TextField.label "Description"
            , TextField.value eventCreationDetails.description
            , Options.onInput <| EventCreationMsg << EC.AdjustEventDescription
            ]
            []
        , Card.actions [ css "display" "flex", css "flex-direction" "row-reverse" ]
            [ Card.actionButtons []
                [ Button.view Mdc
                    "close-event-button"
                    model.mdc
                    [ Card.actionButton
                    , Button.ripple
                    , Options.onClick <| EventCreationMsg EC.CloseUserPromptForEventDetails
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
