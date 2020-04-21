module Session exposing (Session, getKey, getUserId, init, setUserId)

import Browser.Navigation as Nav


type Session
    = Session Nav.Key String


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
