module EventCreation.EventCreation exposing
    ( ConfirmedEventDetails
    , EventCreation(..)
    , EventCreationDetails(..)
    , EventCreationPosition
    , WithDiscardConfirmationModal
    , WithEventCreation
    , areEventCreationsEqual
    , eventDetailsPromptWidth
    )

import Date exposing (Date)


type EventCreation
    = NotCreating
    | CurrentlyCreatingEvent EventCreationDetails EventCreationPosition


type alias WithEventCreation a =
    { a | eventCreation : EventCreation }


type EventCreationDetails
    = UnsetWeeklyFreeTime
    | SetWeeklyFreeTime Int -- weekly free time slot ID
    | ConfirmedEvent ConfirmedEventDetails


type alias ConfirmedEventDetails =
    { eventId : Int
    , recipientIds : List String
    , creatorId : String
    , title : String
    , description : String
    , date : Date
    }


type alias EventCreationPosition =
    { x : Float
    , y : Float
    }


type alias WithDiscardConfirmationModal a =
    { a | isDiscardConfirmationModalOpen : Bool }


areEventCreationsEqual : EventCreationDetails -> EventCreationDetails -> Bool
areEventCreationsEqual eventCreationDetailsA eventCreationDetailsB =
    case ( eventCreationDetailsA, eventCreationDetailsB ) of
        ( UnsetWeeklyFreeTime, UnsetWeeklyFreeTime ) ->
            True

        ( SetWeeklyFreeTime eventA, SetWeeklyFreeTime eventB ) ->
            eventA == eventB

        ( ConfirmedEvent eventA, ConfirmedEvent eventB ) ->
            eventA == eventB

        ( _, _ ) ->
            False


eventDetailsPromptWidth : Float
eventDetailsPromptWidth =
    320
