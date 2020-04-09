module EventCreation.EventCreation exposing
    ( EventCreation(..)
    , EventCreationDetails(..)
    , EventCreationPosition
    , EventItems
    , WithDiscardConfirmationModal
    , WithEventCreation
    , eventDetailsPromptWidth
    )


type EventCreation
    = NotCreating
    | CurrentlyCreatingEvent EventCreationDetails EventCreationPosition


type alias WithEventCreation a =
    { a | eventCreation : EventCreation }


type EventCreationDetails
    = WeeklyFreeTimes
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


eventDetailsPromptWidth : Float
eventDetailsPromptWidth =
    320
