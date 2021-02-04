module Palette exposing (blogHeading, color, heading)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Color exposing (..)

color =
    { primary = rgb255 0 6 255
    , secondary = rgb255 0 242 96
    }


heading : Int -> List (Html msg) -> Html msg
heading level content =
    div [ class <|
            "font-bold " ++
                (case level of
                    1 ->
                        "text-4xl"
                    2 ->
                       "text-2xl"
                    _ ->
                       "text-xl"
                )
        ] content


blogHeading : String -> Html msg
blogHeading title =
    Html.p
        [ class "font-bold text-4xl text-center"]
        [ Html.text title ]
