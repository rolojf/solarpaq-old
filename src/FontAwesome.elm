module FontAwesome exposing (icon, styledIcon)

import Html exposing (Html)
import Html.Attributes as Attr


styledIcon : String -> List (Html.Attribute msg) -> Html msg
styledIcon classString styles =
    [ Html.i [ Attr.class classString ] [] ]
        |> Html.div styles


icon : String -> Html msg
icon classString =
    Html.i [ Attr.class classString ] []

