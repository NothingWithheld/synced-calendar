module EventCreation.EventCreation exposing
    ( EventCreation(..)
    , EventCreationDetails(..)
    , EventCreationPosition
    , EventItems
    , WithDiscardConfirmationModal
    , WithEventCreation
    , areEventCreationsEqual
    , eventDetailsPromptWidth
    )


type EventCreation
    = NotCreating
    | CurrentlyCreatingEvent EventCreationDetails EventCreationPosition


type alias WithEventCreation a =
    { a | eventCreation : EventCreation }


type EventCreationDetails
    = UnsetWeeklyFreeTime
    | SetWeeklyFreeTime Int -- weekly free time slot ID
    | EventDetails EventItems


type alias EventItems =
    { title : String
    , description : String
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

        ( EventDetails eventItemsA, EventDetails eventItemsB ) ->
            eventItemsA == eventItemsB

        ( _, _ ) ->
            False


eventDetailsPromptWidth : Float
eventDetailsPromptWidth =
    320
