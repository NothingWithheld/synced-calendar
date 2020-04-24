module ProposeEvent.Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (Html, div, text)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
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
    , fromDate : Maybe String
    , toDate : Maybe String
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
      , fromDate = Nothing
      , toDate = Nothing
      , mdc = Material.defaultModel
      }
    , Material.init Mdc
    )



-- UPDATE


type Msg
    = AdjustTitle String
    | AdjustDescription String
    | AdjustRecipiecntToBeAdded String
    | AddRecipient
    | RemoveRecipient String
    | UpdateDates DateDetails
    | Mdc (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTitle title ->
            ( { model | title = title }, Cmd.none )

        AdjustDescription description ->
            ( { model | description = description }, Cmd.none )

        AdjustRecipiecntToBeAdded userId ->
            ( { model
                | recipientToBeAdded = userId
                , invalidRecipient = False
              }
            , Cmd.none
            )

        AddRecipient ->
            let
                recipientId =
                    model.recipientToBeAdded

                alreadyIncluded =
                    List.member recipientId model.recipientIds
            in
            if alreadyIncluded || recipientId == "" then
                ( { model | invalidRecipient = True }, Cmd.none )

            else
                ( { model
                    | recipientIds = recipientId :: model.recipientIds
                    , recipientToBeAdded = ""
                  }
                , Cmd.none
                )

        RemoveRecipient recipientIdToRemove ->
            ( { model
                | recipientIds =
                    List.filter
                        ((/=) recipientIdToRemove)
                        model.recipientIds
              }
            , Cmd.none
            )

        UpdateDates { startDate, endDate } ->
            ( { model
                | fromDate = startDate
                , toDate = endDate
              }
            , Cmd.none
            )

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
                []
                [ Button.view Mdc
                    "back-button"
                    model.mdc
                    [ Button.ripple
                    , Button.icon "arrow_back"
                    , css "margin-bottom" "8px"
                    , Button.link <| Route.routeToString Route.Home
                    ]
                    [ text "Back" ]
                , styled div
                    [ css "width" "800px"
                    , Elevation.z8
                    , css "border-radius" "8px"
                    , css "display" "flex"
                    , css "flex-direction" "column"
                    ]
                    [ styled Html.h1
                        [ Typography.headline6
                        , css "margin-left" "16px"
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
                    , styled div
                        [ css "display" "flex"
                        , css "justify-content" "flex-end"
                        , css "margin" "12px"
                        ]
                        [ Button.view
                            Mdc
                            "submit-proposed-event-button"
                            model.mdc
                            [ Button.ripple
                            , Button.unelevated
                            ]
                            [ text "Submit" ]
                        ]
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
        , css "flex" "3"
        , css "margin-left" "8px"
        , css "margin-right" "12px"
        ]
        [ TextField.view Mdc
            "title-text-field"
            model.mdc
            [ TextField.label "Title"
            , TextField.value model.title
            , when model.invalidTitle TextField.invalid
            , Options.onInput AdjustTitle
            , css "margin-bottom" "12px"
            ]
            []
        , viewDatePicker "propose-event-datepicker"
            model.fromDate
            model.toDate
            UpdateDates
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


type alias DateDetails =
    { startDate : Maybe String
    , endDate : Maybe String
    }


dateDetailsDecoder : Decoder DateDetails
dateDetailsDecoder =
    Decode.field "detail" <|
        Decode.map2 DateDetails
            (Decode.field "startDate" <| Decode.nullable Decode.string)
            (Decode.field "endDate" <| Decode.nullable Decode.string)


viewDatePicker : String -> Maybe String -> Maybe String -> (DateDetails -> Msg) -> Html Msg
viewDatePicker id startDate endDate onDateChange =
    let
        dateEncoder date =
            case date of
                Just dateString ->
                    Encode.string dateString

                Nothing ->
                    Encode.null
    in
    Html.node "custom-datepicker"
        [ Attributes.property "id" <| Encode.string id
        , Attributes.property "dates" <|
            Encode.list dateEncoder [ startDate, endDate ]
        , Events.on "onDateChange" <| Decode.map onDateChange dateDetailsDecoder
        ]
        []


viewRightOfFold : Model -> Html Msg
viewRightOfFold model =
    styled div
        [ css "flex" "2"
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
                , css "flex" "8"
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
                [ css "flex" "1"
                , css "margin" "4px"
                , Button.ripple
                , Options.onClick AddRecipient
                ]
                [ text "Add" ]
            ]
        , Chip.chipset [ Chip.input ] <|
            List.map (viewRecipientChip model)
                model.recipientIds
        ]


viewRecipientChip : Model -> String -> Html Msg
viewRecipientChip model userId =
    Chip.view Mdc
        ("recipient--" ++ userId)
        model.mdc
        [ Chip.trailingIcon "close"
        , Chip.onClick <| RemoveRecipient userId
        ]
        [ text userId ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model
