module Error exposing (view404)

import Browser exposing (Document)
import Html exposing (text)
import Material.Options exposing (css, styled)
import Material.Typography as Typography


view404 : Document msg
view404 =
    { title = "404 Not Found"
    , body =
        [ styled Html.h1
            [ Typography.headline1
            , css "text-align" "center"
            ]
            [ text "404" ]
        ]
    }
