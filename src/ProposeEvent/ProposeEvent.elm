module ProposeEvent.ProposeEvent exposing
    ( ProposedEvent
    , WithAlreadySubmittedAvailability
    , WithProposedEvent
    )

import Date exposing (Date)


type alias ProposedEvent =
    { title : String
    , description : String
    , creatorId : String
    , eventId : Int
    , fromDate : Date
    , toDate : Date
    }


type alias WithProposedEvent a =
    { a
        | proposedEvent : Maybe ProposedEvent
    }


type alias WithAlreadySubmittedAvailability a =
    { a
        | alreadySubmittedAvailability : Bool
    }
