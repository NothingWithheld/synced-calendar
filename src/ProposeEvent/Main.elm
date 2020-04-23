module ProposeEvent.Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (Html, div, text)
import Material
import Material.Button as Button
import Material.Chip as Chip
import Material.Elevation as Elevation
import Material.Options as Options exposing (css, styled, when)
import Material.TextField as TextField
import Material.Typography as Typography
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , title : String
    , invalidTitle : Bool
    , description : String
    , recipientIds : List String
    , recipientToBeAdded : String
    , invalidRecipient : Bool
    , mdc : Material.Model Msg
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , title = ""
      , invalidTitle = False
      , description = ""
      , recipientIds = []
      , recipientToBeAdded = ""
      , invalidRecipient = False
      , mdc = Material.defaultModel
      }
    , Material.init Mdc
    )



-- UPDATE


type Msg
    = AdjustTitle String
    | AdjustDescription String
    | AdjustRecipiecntToBeAdded String
    | Mdc (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTitle title ->
            ( { model | title = title }, Cmd.none )

        AdjustDescription description ->
            ( { model | description = description }, Cmd.none )

        AdjustRecipiecntToBeAdded userId ->
            ( { model | recipientToBeAdded = userId }, Cmd.none )

        Mdc msg_ ->
            Material.update Mdc msg_ model



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Synced Calendar - Home"
    , body =
        [ styled div
            [ css "display" "flex"
            , css "justify-content" "center"
            , css "align-items" "center"
            , css "min-height" "600px"
            , css "height" "100%"
            ]
            [ styled div
                [ css "width" "800px"
                , Elevation.z8
                , css "border-radius" "8px"
                , css "display" "flex"
                , css "flex-direction" "column"
                ]
                [ styled Html.h1
                    [ Typography.headline6
                    , css "margin-left" "12px"
                    ]
                    [ text "Create Event"
                    ]
                , styled div
                    [ css "display" "flex"
                    , css "justify-content" "space-between"
                    ]
                    [ viewLeftOfFold model
                    , viewRightOfFold model
                    ]
                ]
            ]
        ]
    }


viewLeftOfFold : Model -> Html Msg
viewLeftOfFold model =
    styled div
        [ css "display" "flex"
        , css "flex-direction" "column"
        , css "flex-grow" "3"
        , css "margin-right" "12px"
        ]
        [ TextField.view Mdc
            "title-text-field"
            model.mdc
            [ TextField.label "Title"
            , TextField.value model.title
            , when model.invalidTitle TextField.invalid
            , Options.onInput AdjustTitle
            ]
            []
        , TextField.view
            Mdc
            "description-text-field"
            model.mdc
            [ TextField.label "Description"
            , TextField.value model.description
            , TextField.textarea
            , TextField.rows 3
            , Options.onInput AdjustDescription
            , css "margin-top" "12px"
            ]
            []
        ]


viewRightOfFold : Model -> Html Msg
viewRightOfFold model =
    styled div
        [ css "flex-grow" "2"
        , css "display" "flex"
        , css "flex-direction" "column"
        ]
        [ styled div
            [ css "display" "flex"
            , css "justify-content" "space-between"
            , css "align-items" "center"
            ]
            [ styled div
                [ css "display" "flex"
                , css "flex-direction" "column"
                , css "flex-grow" "8"
                ]
                [ TextField.view Mdc
                    "adding-recipient-text-field"
                    model.mdc
                    [ TextField.label "User to Add"
                    , TextField.value model.recipientToBeAdded
                    , when model.invalidRecipient TextField.invalid
                    , Options.onInput AdjustRecipiecntToBeAdded
                    ]
                    []
                ]
            , Button.view Mdc
                "add-recipient-button"
                model.mdc
                [ css "flex-grow" "1"
                , css "margin" "4px"
                , Button.ripple
                ]
                [ text "Add" ]
            ]
        , Chip.chipset [ Chip.input ] []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model
