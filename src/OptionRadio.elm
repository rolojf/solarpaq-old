module OptionRadio exposing (..)

import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class)
import Html.Attributes.Aria as Aria
import Html.Events


type alias Option varianteTipo =
    { descripcion : String
    , nota : String
    , name : String
    , id : String
    , tipo : varianteTipo
    , extremo : Bool
    }


type alias Model resultadoEsperado =
    { seleccionado : resultadoEsperado }


init : resultadoEsperado -> Model resultadoEsperado
init resultadoEsperado =
    { seleccionado = resultadoEsperado }


type alias Msg resultadoEsperado =
    resultadoEsperado


update cualOpcion model =
    { model | seleccionado = cualOpcion }


view : String -> Model varianteTipo -> List (Option varianteTipo) -> Html varianteTipo
view queOpcionDamos model opciones =
    Html.fieldset []
        [ Html.legend
            [ class "sr-only" ]
            [ Html.text queOpcionDamos ]
        , div
            [ class "bg-white rounded-md -space-y-px max-w-md" ]
            (opciones
                |> List.map (viewOption model)
            )
        ]


viewOption : Model resultadoEsperado -> Option resultadoEsperado -> Html resultadoEsperado
viewOption model radioOp =
    let
        claseSegunSeleccion : String
        claseSegunSeleccion =
            case ( radioOp.tipo == model.seleccionado, radioOp.extremo ) of
                ( True, False ) ->
                    "relative border bg-indigo-50 border-indigo-200 z-10 rounded-bl-md rounded-br-md p-4 flex"

                ( False, False ) ->
                    "relative border border-gray-200 p-4 flex"

                ( True, True ) ->
                    "relative border bg-indigo-50 border-indigo-200 z-10 rounded-bl-md rounded-br-md p-4 flex"

                ( False, True ) ->
                    "relative border border-gray-200 rounded-bl-md rounded-br-md p-4 flex"
    in
    div
        [ class claseSegunSeleccion ]
        [ div
            [ class "flex items-center h-5" ]
            [ Html.input
                [ Attr.id radioOp.id
                , Attr.name radioOp.name
                , Attr.type_ "radio"
                , Html.Events.onCheck (\_ -> radioOp.tipo)
                , class "focus:ring-indigo-500 h-4 w-4 text-indigo-600 cursor-pointer border-gray-300"
                ]
                []
            ]
        , Html.label
            [ Attr.for radioOp.id
            , class "ml-3 flex flex-col cursor-pointer"
            ]
            [ Html.span
                [ class <|
                    if radioOp.tipo == model.seleccionado then
                        "text-indigo-900"

                    else
                        "text-gray-900"
                ]
                [ Html.span
                    [ class "block text-sm font-medium" ]
                    [ Html.text radioOp.descripcion ]
                , Html.span
                    [ class <|
                        if radioOp.tipo == model.seleccionado then
                            "text-indigo-700"

                        else
                            "text-gray-500"
                    , class "block text-sm"
                    ]
                    [ Html.text radioOp.nota ]
                ]
            ]
        ]
