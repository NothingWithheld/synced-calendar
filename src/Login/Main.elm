module Login.Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (div, text)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Options as Options exposing (css, styled)
import Material.TextField as TextField
import Material.Typography as Typography
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , mdc : Material.Model Msg
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , mdc = Material.defaultModel
      }
    , Material.init Mdc
    )



-- UPDATE


type Msg
    = AdjustUserId String
    | Mdc (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustUserId userId ->
            let
                updatedSession =
                    Session.setUserId model.session userId
            in
            ( { model | session = updatedSession }, Cmd.none )

        Mdc msg_ ->
            Material.update Mdc msg_ model



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Synced Calendar - Log In"
    , body =
        [ styled div
            [ css "display" "flex"
            , css "justify-content" "center"
            , css "align-items" "center"
            , css "min-height" "600px"
            , css "height" "100%"
            ]
            [ Card.view [ css "width" "450px" ]
                [ styled div
                    [ css "padding" "4px 16px" ]
                    [ styled Html.h1
                        [ Typography.headline5
                        ]
                        [ text "Log In" ]
                    ]
                , TextField.view Mdc
                    "user-id"
                    model.mdc
                    [ TextField.label "User ID"
                    , TextField.value <| Session.getUserId model.session
                    , Options.onInput AdjustUserId
                    ]
                    []
                , Card.actions
                    [ css "display" "flex"
                    , css "flex-direction" "row-reverse"
                    , css "margin-top" "8px"
                    ]
                    [ Card.actionButtons []
                        [ Button.view Mdc
                            "login-button"
                            model.mdc
                            [ Card.actionButton
                            , Button.ripple
                            , Button.unelevated
                            , css "width" "100px"
                            , Button.link <| Route.routeToString Route.WeeklyFreeTimes
                            ]
                            [ text "Log In" ]
                        ]
                    ]
                ]
            ]
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model
