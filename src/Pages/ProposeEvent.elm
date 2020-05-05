module Pages.ProposeEvent exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (Html, div, text)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Material
import Material.Button as Button
import Material.Chip as Chip
import Material.Elevation as Elevation
import Material.Options as Options exposing (css, styled, when)
import Material.TextField as TextField
import Material.Typography as Typography
import ProposeEvent.Commands exposing (submitEventProposal)
import Route
import Session exposing (Session)
import Utils exposing (NoData)



-- MODEL


type alias Model =
    { session : Session
    , title : String
    , invalidTitle : Bool
    , description : String
    , recipientEmails : List String
    , recipientToBeAdded : String
    , invalidRecipient : Bool
    , noRecipientAfterSubmission : Bool
    , fromDate : Maybe String
    , toDate : Maybe String
    , invalidDates : Bool
    , eventProposalSuccess : Bool
    , mdc : Material.Model Msg
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , title = ""
      , invalidTitle = False
      , description = ""
      , recipientEmails = []
      , recipientToBeAdded = ""
      , invalidRecipient = False
      , noRecipientAfterSubmission = False
      , fromDate = Nothing
      , toDate = Nothing
      , invalidDates = False
      , eventProposalSuccess = False
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
    | AttemptProposalSubmit
    | OnSubmitResult (Result Http.Error NoData)
    | Mdc (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTitle title ->
            ( { model
                | title = title
                , invalidTitle = False
              }
            , Cmd.none
            )

        AdjustDescription description ->
            ( { model | description = description }, Cmd.none )

        AdjustRecipiecntToBeAdded userId ->
            ( { model
                | recipientToBeAdded = userId
                , invalidRecipient = False
                , noRecipientAfterSubmission = False
              }
            , Cmd.none
            )

        AddRecipient ->
            let
                recipientId =
                    model.recipientToBeAdded

                alreadyIncluded =
                    List.member recipientId model.recipientEmails

                isSelf =
                    recipientId == Session.getUserId model.session
            in
            if alreadyIncluded || isSelf || recipientId == "" then
                ( { model | invalidRecipient = True }, Cmd.none )

            else
                ( { model
                    | recipientEmails = recipientId :: model.recipientEmails
                    , recipientToBeAdded = ""
                    , noRecipientAfterSubmission = False
                  }
                , Cmd.none
                )

        RemoveRecipient recipientIdToRemove ->
            ( { model
                | recipientEmails =
                    List.filter
                        ((/=) recipientIdToRemove)
                        model.recipientEmails
              }
            , Cmd.none
            )

        UpdateDates { startDate, endDate } ->
            ( { model
                | fromDate = startDate
                , toDate = endDate
                , invalidDates = False
              }
            , Cmd.none
            )

        AttemptProposalSubmit ->
            let
                invalidTitle =
                    model.title == ""

                invalidDates =
                    Maybe.withDefault True <|
                        Maybe.map2 (\_ _ -> False) model.fromDate model.toDate

                userId =
                    Session.getUserId model.session
            in
            if List.length model.recipientEmails == 0 then
                ( { model
                    | invalidTitle = invalidTitle
                    , invalidDates = invalidDates
                    , noRecipientAfterSubmission = True
                  }
                , Cmd.none
                )

            else if not invalidTitle && not invalidDates then
                ( model
                , submitEventProposal OnSubmitResult
                    userId
                    model.recipientEmails
                    model.fromDate
                    model.toDate
                    model.title
                    model.description
                )

            else
                ( { model
                    | invalidTitle = invalidTitle
                    , invalidDates = invalidDates
                  }
                , Cmd.none
                )

        OnSubmitResult result ->
            case result of
                Ok _ ->
                    ( { model | eventProposalSuccess = True }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

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
                [ Route.viewHomeButton model Mdc
                , styled div
                    [ css "width" "800px"
                    , Elevation.z8
                    , css "border-radius" "8px"
                    , css "display" "flex"
                    , css "flex-direction" "column"
                    , css "margin-top" "8px"
                    ]
                    (if model.eventProposalSuccess then
                        viewProposalSuccessDialog

                     else
                        viewEventProposalForm model
                    )
                ]
            ]
        ]
    }


viewProposalSuccessDialog : List (Html Msg)
viewProposalSuccessDialog =
    [ styled Html.h1
        [ Typography.headline5
        , css "margin-left" "16px"
        ]
        [ text "Successfully sent out requests to all recipients"
        ]
    ]


viewEventProposalForm : Model -> List (Html Msg)
viewEventProposalForm model =
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
            , Options.onClick AttemptProposalSubmit
            ]
            [ text "Submit" ]
        ]
    ]


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
        , viewDatePicker model
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


viewDatePicker : Model -> Html Msg
viewDatePicker model =
    let
        dateEncoder date =
            case date of
                Just dateString ->
                    Encode.string dateString

                Nothing ->
                    Encode.null
    in
    styled div
        [ when model.invalidDates <| css "border" "solid 1px #D64545" ]
        [ Html.node "custom-datepicker"
            [ Attributes.property "id" <| Encode.string "propose-event-datepicker"
            , Attributes.property "dates" <|
                Encode.list dateEncoder [ model.fromDate, model.toDate ]
            , Events.on "onDateChange" <| Decode.map UpdateDates dateDetailsDecoder
            ]
            []
        ]


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
            , when model.noRecipientAfterSubmission <|
                css "border" "solid 1px #D64545"
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
                model.recipientEmails
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
