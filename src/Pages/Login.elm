module Pages.Login exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Constants
import Html exposing (Html, div, text)
import Http
import Json.Decode as Decode exposing (Decoder)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Elevation as Elevation
import Material.Options as Options exposing (css, styled)
import Material.TextField as TextField
import Material.Typography as Typography
import Route
import Session exposing (Session)
import Url.Builder as Builder
import Utils exposing (NoData)



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
    = AdjustLoginEmail String
    | AdjustLoginPassword String
    | SwitchToCreateAccount
    | AdjustCreateAccountEmail String
    | AdjustCreateAccountPassword String
    | SubmitAccountCreation
    | HandleAccountCreation (Result Http.Error NoData)
    | SubmitLogin
    | HandleLogin (Result Http.Error String)
    | Mdc (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustLoginEmail email ->
            let
                updatedSession =
                    Session.setEmail model.session email
            in
            ( { model
                | session = updatedSession
                , errorMessage = ""
              }
            , Cmd.none
            )

        AdjustLoginPassword loginPassword ->
            ( { model
                | loginPassword = loginPassword
                , errorMessage = ""
              }
            , Cmd.none
            )

        SwitchToCreateAccount ->
            ( { model
                | createEmail = ""
                , createPassword = ""
                , isCreatingAccount = True
              }
            , Cmd.none
            )

        AdjustCreateAccountEmail createEmail ->
            ( { model
                | createEmail = createEmail
                , errorMessage = ""
              }
            , Cmd.none
            )

        AdjustCreateAccountPassword createPassword ->
            ( { model | createPassword = createPassword }, Cmd.none )

        SubmitAccountCreation ->
            ( model, createAccount model )

        HandleAccountCreation result ->
            let
                updatedSession =
                    Session.setEmail model.session ""
            in
            case result of
                Ok _ ->
                    ( { model
                        | session = updatedSession
                        , loginPassword = ""
                        , isCreatingAccount = False
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | errorMessage = "There's already an account with this email" }, Cmd.none )

        SubmitLogin ->
            ( model, login model )

        HandleLogin result ->
            case result of
                Ok userId ->
                    let
                        updatedSession =
                            Session.setUserId model.session userId
                    in
                    ( { model | session = updatedSession }
                    , Route.pushUrl (Session.getKey model.session) Route.Home
                    )

                Err _ ->
                    ( { model | errorMessage = "Invalid credentials" }, Cmd.none )

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
        , if String.length model.errorMessage > 0 then
            styled Html.p
                [ css "margin-top" "0"
                , css "padding-left" "16px"
                , css "color" Constants.invalidColor
                ]
                [ text model.errorMessage ]

          else
            text ""
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
                    , Options.onClick SubmitLogin
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
        , if String.length model.errorMessage > 0 then
            styled Html.p
                [ css "margin-top" "0"
                , css "padding-left" "16px"
                , css "color" Constants.invalidColor
                ]
                [ text model.errorMessage ]

          else
            text ""
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
                    , Options.onClick SubmitAccountCreation
                    ]
                    [ text "Create Account" ]
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model



-- MESSAGING


createAccount : Model -> Cmd Msg
createAccount model =
    let
        queryString =
            String.dropLeft 1 <|
                Builder.toQuery
                    [ Builder.string "email" model.createEmail
                    , Builder.string "password" model.createPassword
                    ]
    in
    Http.post
        { url = "http://localhost:3000/api/user/login"
        , body = Http.stringBody "application/x-www-form-urlencoded" queryString
        , expect = Http.expectJson HandleAccountCreation Utils.noDataDecoder
        }


login : Model -> Cmd Msg
login model =
    let
        queryString =
            Builder.toQuery
                [ Builder.string "email" <| Session.getEmail model.session
                , Builder.string "password" model.loginPassword
                ]
    in
    Http.get
        { url = "http://localhost:3000/api/user/login" ++ queryString
        , expect = Http.expectJson HandleLogin loginDecoder
        }


loginDecoder : Decoder String
loginDecoder =
    Decode.map String.fromInt <| Decode.field "userId" Decode.int
