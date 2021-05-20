module OptionRadio exposing (..)

import Css
import Html.Styled as Htmls exposing (div)
import Html.Styled.Attributes as Attr exposing (class)
import Html.Styled.Events as Events
import Tailwind.Breakpoints as TwBp
import Tailwind.Utilities as Tw


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


view : String -> Model varianteTipo -> List (Option varianteTipo) -> Htmls.Html varianteTipo
view queOpcionDamos model opciones =
    Htmls.fieldset []
        [ Htmls.legend
            [ Attr.css [ Tw.sr_only ] ]
            [ Htmls.text queOpcionDamos ]
        , div
            [ Attr.css [ Tw.bg_white, Tw.rounded_md, Tw.neg_space_y_px, Tw.max_w_md ] ]
            (opciones
                |> List.map (viewOption model)
            )
        ]


viewOption : Model resultadoEsperado -> Option resultadoEsperado -> Htmls.Html resultadoEsperado
viewOption model radioOp =
    let
        claseSegunSeleccion : List Css.Style
        claseSegunSeleccion =
            case ( radioOp.tipo == model.seleccionado, radioOp.extremo ) of
                ( True, False ) ->
                    [ Tw.relative, Tw.border, Tw.bg_indigo_50, Tw.border_indigo_200, Tw.z_10, Tw.rounded_bl_md, Tw.rounded_br_md, Tw.p_4, Tw.flex ]

                ( False, False ) ->
                    [ Tw.relative, Tw.border, Tw.border_gray_200, Tw.p_4, Tw.flex ]

                ( True, True ) ->
                    [ Tw.relative, Tw.border, Tw.bg_indigo_50, Tw.border_indigo_200, Tw.z_10, Tw.rounded_bl_md, Tw.rounded_br_md, Tw.p_4, Tw.flex ]

                ( False, True ) ->
                    [ Tw.relative, Tw.border, Tw.border_gray_200, Tw.rounded_bl_md, Tw.rounded_br_md, Tw.p_4, Tw.flex ]
    in
    div
        [ Attr.css claseSegunSeleccion ]
        [ div
            [ Attr.css [ Tw.flex, Tw.items_center, Tw.h_5 ] ]
            [ Htmls.input
                [ Attr.id radioOp.id
                , Attr.name radioOp.name
                , Attr.type_ "radio"
                , Events.onCheck (\_ -> radioOp.tipo)
                , Attr.css [ Css.focus [ Tw.ring_indigo_500 ], Tw.h_4, Tw.w_4, Tw.text_indigo_600, Tw.cursor_pointer, Tw.border_gray_300 ]
                ]
                []
            ]
        , Htmls.label
            [ Attr.for radioOp.id
            , Attr.css [ Tw.ml_3, Tw.flex, Tw.flex_col, Tw.cursor_pointer ]
            ]
            [ Htmls.span
                [ Attr.css <|
                    if radioOp.tipo == model.seleccionado then
                        [ Tw.text_indigo_900 ]

                    else
                        [ Tw.text_gray_900 ]
                ]
                [ Htmls.span
                    [ Attr.css [ Tw.block, Tw.text_sm, Tw.font_medium ] ]
                    [ Htmls.text radioOp.descripcion ]
                , Htmls.span
                    [ Attr.css <|
                        List.append
                            [ Tw.block, Tw.text_sm ]
                            (if radioOp.tipo == model.seleccionado then
                                [ Tw.text_indigo_700 ]

                             else
                                [ Tw.text_gray_500 ]
                            )
                    ]
                    [ Htmls.text radioOp.nota ]
                ]
            ]
        ]
