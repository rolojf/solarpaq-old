module Dotted exposing (lines)

import Html exposing (Html)
import Svg
import Svg.Attributes as Attr



lines =
    [ Svg.svg
        [ Attr.width "20"
        , Attr.height "30"
        , Attr.viewBox "0 0 20 30"
        ]
        [ Svg.path
            [ Attr.stroke "#2a75ff"
            , Attr.strokeWidth "4"
            , Attr.strokeLinecap "round"
            , Attr.strokeDasharray "0.5 10"
            , Attr.d "M10 40 L10 -10"
            , Attr.class "dotted-line"
            ]
            []
        ]
    ]
    |> Html.div
        [ Attr.class "text-center" ]
