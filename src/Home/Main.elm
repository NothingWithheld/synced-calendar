module Home.Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (Html, div, text)
import Material
import Material.Button as Button
import Material.Elevation as Elevation
import Material.Options exposing (css, styled)
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
    = Mdc (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            [ styled div
                [ css "width" "450px"
                , Elevation.z8
                , css "border-radius" "8px"
                ]
                [ viewLineEntry model
                    "Edit when you're free each week"
                    "weekly-free-times-button"
                  <|
                    Route.routeToString Route.WeeklyFreeTimes
                , styled Html.hr
                    [ css "margin" "0" ]
                    []
                , viewLineEntry
                    model
                    "Create and invite people to an event"
                    "create-event-button"
                  <|
                    Route.routeToString Route.WeeklyFreeTimes
                ]
            ]
        ]
    }


viewLineEntry : Model -> String -> String -> String -> Html Msg
viewLineEntry model description buttonLabel href =
    styled div
        [ css "padding" "4px 16px"
        , css "display" "flex"
        , css "justify-content" "space-between"
        , css "align-items" "center"
        ]
        [ styled Html.p
            [ Typography.body1
            ]
            [ text description ]
        , Button.view Mdc
            buttonLabel
            model.mdc
            [ Button.ripple
            , Button.link href
            , Button.icon "arrow_forward"
            ]
            [ text "Ok" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model
