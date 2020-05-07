module Pages.Login exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (Html, div, text)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Elevation as Elevation
import Material.Options as Options exposing (css, styled)
import Material.TextField as TextField
import Material.Typography as Typography
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , mdc : Material.Model Msg
    , loginPassword : String
    , createEmail : String
    , createPassword : String
    , errorMessage : String
    , isCreatingAccount : Bool
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , mdc = Material.defaultModel
      , loginPassword = ""
      , createEmail = ""
      , createPassword = ""
      , errorMessage = ""
      , isCreatingAccount = False
      }
    , Material.init Mdc
    )



-- UPDATE


type Msg
    = AdjustUserId String
    | AdjustLoginEmail String
    | AdjustLoginPassword String
    | SwitchToCreateAccount
    | AdjustCreateAccountEmail String
    | AdjustCreateAccountPassword String
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

        AdjustLoginEmail email ->
            let
                updatedSession =
                    Session.setEmail model.session email
            in
            ( { model | session = updatedSession }, Cmd.none )

        AdjustLoginPassword loginPassword ->
            ( { model | loginPassword = loginPassword }, Cmd.none )

        SwitchToCreateAccount ->
            ( { model
                | createEmail = ""
                , createPassword = ""
                , isCreatingAccount = True
              }
            , Cmd.none
            )

        AdjustCreateAccountEmail createEmail ->
            ( { model | createEmail = createEmail }, Cmd.none )

        AdjustCreateAccountPassword createPassword ->
            ( { model | createPassword = createPassword }, Cmd.none )

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
            [ if model.isCreatingAccount then
                viewCreateAccount model

              else
                viewLogin model
            ]
        ]
    }


viewLogin : Model -> Html Msg
viewLogin model =
    Card.view [ css "width" "450px", Elevation.z8 ]
        [ styled div
            [ css "padding" "4px 16px" ]
            [ styled Html.h1
                [ Typography.headline5
                ]
                [ text "Log In" ]
            ]
        , TextField.view Mdc
            "email"
            model.mdc
            [ TextField.label "Email"
            , TextField.value <| Session.getEmail model.session
            , Options.onInput AdjustLoginEmail
            ]
            []
        , TextField.view Mdc
            "password"
            model.mdc
            [ TextField.label "Password"
            , TextField.value <| String.map (\_ -> '*') model.loginPassword
            , Options.onInput AdjustLoginPassword
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
                    , Button.link <| Route.routeToString Route.Home
                    ]
                    [ text "Log In" ]
                ]
            , Card.actionButtons []
                [ Button.view Mdc
                    "switch-to-create-account-button"
                    model.mdc
                    [ Card.actionButton
                    , Button.ripple
                    , Options.onClick SwitchToCreateAccount
                    , css "margin-right" "8px"
                    ]
                    [ text "Create Account" ]
                ]
            ]
        ]


viewCreateAccount : Model -> Html Msg
viewCreateAccount model =
    Card.view [ css "width" "450px", Elevation.z8 ]
        [ styled div
            [ css "padding" "4px 16px" ]
            [ styled Html.h1
                [ Typography.headline5
                ]
                [ text "Create Account" ]
            ]
        , TextField.view Mdc
            "email"
            model.mdc
            [ TextField.label "Email"
            , TextField.value model.createEmail
            , Options.onInput AdjustCreateAccountEmail
            ]
            []
        , TextField.view Mdc
            "password"
            model.mdc
            [ TextField.label "Password"
            , TextField.value <| String.map (\_ -> '*') model.createPassword
            , Options.onInput AdjustCreateAccountPassword
            ]
            []
        , Card.actions
            [ css "display" "flex"
            , css "flex-direction" "row-reverse"
            , css "margin-top" "8px"
            ]
            [ Card.actionButtons []
                [ Button.view Mdc
                    "create-account-button"
                    model.mdc
                    [ Card.actionButton
                    , Button.ripple
                    , Button.unelevated
                    , Button.link <| Route.routeToString Route.Home
                    ]
                    [ text "Create Account" ]
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model
