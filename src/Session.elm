module Session exposing (Session, WithSession, getKey, getUserId, hasUserId, init, setUserId)

import Browser.Navigation as Nav


type Session
    = Session Nav.Key String


type alias WithSession a =
    { a
        | session : Session
    }


init : Nav.Key -> Session
init key =
    Session key ""


getUserId : Session -> String
getUserId (Session _ userId) =
    userId


setUserId : Session -> String -> Session
setUserId (Session key _) userId =
    Session key userId


getKey : Session -> Nav.Key
getKey (Session key _) =
    key


hasUserId : Session -> Bool
hasUserId (Session _ userId) =
    userId /= ""
