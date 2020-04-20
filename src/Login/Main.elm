module Login.Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (div, text)
import Material
import Material.Card as Card
import Material.Options as Options exposing (css, styled)
import Material.TextField as TextField
import Material.Typography as Typography



-- MODEL


type alias Model =
    { userId : String
    , mdc : Material.Model Msg
    }


init : ( Model, Cmd Msg )
init =
    ( { userId = ""
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
            ( { model | userId = userId }, Cmd.none )

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
                    , TextField.value model.userId
                    , Options.onInput AdjustUserId
                    ]
                    []
                ]
            ]
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model
