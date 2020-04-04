module EventCreation.EventCreation exposing
    ( EventCreation(..)
    , EventCreationDetails
    , EventCreationPosition
    , Msg(..)
    , WithEventCreation
    , eventDetailsPromptWidth
    )

import Browser.Dom as Dom


type EventCreation
    = NotCreating
    | CurrentlyCreatingEvent EventCreationDetails EventCreationPosition


type alias WithEventCreation a =
    { a | eventCreation : EventCreation }


type alias EventCreationDetails =
    { title : String
    , description : String
    }


type alias EventCreationPosition =
    { x : Float
    , y : Float
    }


eventDetailsPromptWidth : Float
eventDetailsPromptWidth =
    300


type Msg
    = SetOneHourSelection Int Int
    | InitiateUserPromptForEventDetails
    | PromptUserForEventDetails (Result Dom.Error Dom.Element)
    | AdjustEventTitle String
    | AdjustEventDescription String
    | CloseUserPromptForEventDetails
