module Template.Home exposing (Model, Msg, decoder, template)

import Browser.Dom as Dom
import Cloudinary
import Css
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Attributes.Aria as Aria
import Html.Styled as Htmls exposing (div, text)
import Html.Styled.Attributes as Attr exposing (class)
import Html.Styled.Events as Events
import Json.Decode as Decode
import MarkdownRenderer
import Pages exposing (images)
import Pages.PagePath as PagePath exposing (PagePath)
import Pages.StaticHttp as StaticHttp
import Shared exposing (UsuarioStatus)
import Svg.Styled as Svg exposing (svg, path)
import Svg.Styled.Attributes as SvgAttr
import Site
import Tailwind.Breakpoints as TwBp
import Tailwind.Utilities as Tw
import Template exposing (StaticPayload, TemplateWithState)
import TemplateMetadata exposing (Home)
import TemplateType exposing (TemplateType)


type alias StaticData =
    ()


type alias Model =
    {}


type Msg
    = Increment


template : TemplateWithState Home StaticData Model Msg
template =
    Template.noStaticData { head = head }
        |> Template.buildWithSharedState
            { view = view
            , init = init
            , update = update
            , subscriptions = \_ _ _ _ -> Sub.none
            }


init : Home -> ( Model, Cmd Msg )
init metadata =
    ( {}
    , Cmd.none
    )


update : Home -> Msg -> Model -> Shared.Model -> ( Model, Cmd Msg, Maybe Shared.SharedMsg )
update metadata msg model sharedModel =
    case msg of
        Increment ->
            ( model, Cmd.none, Just Shared.IncrementFromChild )


staticData :
    List ( PagePath Pages.PathKey, TemplateType )
    -> StaticHttp.Request StaticData
staticData siteMetadata =
    StaticHttp.succeed ()


decoder : Decode.Decoder Home
decoder =
    Decode.map2 Home
        (Decode.field "title" Decode.string)
        (Decode.succeed
            -- definir aquí como va el menú
            [ { direccion = "once"
              , queDice = "eleven"
              }
            , { direccion = "doce"
              , queDice = "Twuelve"
              }
            ]
        )


head : StaticPayload Home StaticData -> List (Head.Tag Pages.PathKey)
head staticPayload =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "SOLARPAQ"
        , image =
            { url = images.iconPng
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = Site.tagline
        , locale = Nothing
        , title = staticPayload.metadata.title
        }
        |> Seo.website


view :
    Model
    -> Shared.Model
    -> List ( PagePath Pages.PathKey, TemplateType )
    -> StaticPayload Home StaticData
    -> Shared.RenderedBody
    -> Shared.PageView Msg
view model sharedModel allMetadata staticPayload rendered =
    { title = staticPayload.metadata.title
    , body =
        [ div
            [ ]
            [ -- counterView sharedModel
            {-, Htmls.br [] []
            , Htmls.text "Aquí estaba el listado de la documentación"
            , div [ class "segunda columna" ]
                [ div
                    [ Attr.css [ Tw.prose, TwBp.lg [ Tw.prose_xl ] ] ]
                    (Tuple.second rendered
                        |> List.map (Html.map never)
                        |> List.map Htmls.fromUnstyled
                    )
                ]
            , -}
            viewAboveTheFold
            ]
            |> Htmls.toUnstyled
        ]
    }


counterView : Shared.Model -> Htmls.Html Msg
counterView sharedModel =
    Htmls.text <| "Docs count: " ++ String.fromInt sharedModel.counter


viewAboveTheFold : Htmls.Html msg
viewAboveTheFold =
    div
                [ Attr.css
                    [ Tw.relative
                    , Tw.bg_white
                    , Tw.overflow_hidden
                    ]
                ]
                [ div
                    [ Attr.css
                        [ Tw.max_w_7xl
                        , Tw.mx_auto
                        ]
                    ]
                    [ div
                        [ Attr.css
                            [ Tw.relative
                            , Tw.z_10
                            , Tw.pb_8
                            , Tw.bg_white
                            , TwBp.lg
                                [ Tw.max_w_2xl
                                , Tw.w_full
                                , Tw.pb_28
                                ]
                            , TwBp.md
                                [ Tw.pb_20
                                ]
                            , TwBp.sm
                                [ Tw.pb_16
                                ]
                            , TwBp.xl
                                [ Tw.pb_32
                                ]
                            ]
                        ]
                        [ svg
                            [ SvgAttr.css
                                [ Tw.hidden
                                , Tw.absolute
                                , Tw.right_0
                                , Tw.inset_y_0
                                , Tw.h_full
                                , Tw.w_48
                                , Tw.text_white
                                , Tw.transform
                                , Tw.translate_x_1over2
                                , TwBp.lg
                                    [ Tw.block
                                    ]
                                ]
                            , SvgAttr.fill "currentColor"
                            , SvgAttr.viewBox "0 0 100 100"
                            , SvgAttr.preserveAspectRatio "none"
                            , Attr.attribute "aria-hidden" "true"
                            ]
                            [ Svg.polygon
                                [ SvgAttr.points "50,0 100,0 50,100 0,100"
                                ]
                                []
                            ]
                        , div
                            [ Attr.css
                                [ Tw.relative
                                , Tw.pt_6
                                , Tw.px_4
                                , TwBp.lg
                                    [ Tw.px_8
                                    ]
                                , TwBp.sm
                                    [ Tw.px_6
                                    ]
                                ]
                            ]
                            [ Htmls.nav
                                [ Attr.css
                                    [ Tw.relative
                                    , Tw.flex
                                    , Tw.items_center
                                    , Tw.justify_between
                                    , TwBp.lg
                                        [ Tw.justify_start
                                        ]
                                    , TwBp.sm
                                        [ Tw.h_10
                                        ]
                                    ]
                                , Attr.attribute "aria-label" "Global"
                                ]
                                [ div
                                    [ Attr.css
                                        [ Tw.flex
                                        , Tw.items_center
                                        , Tw.flex_grow
                                        , Tw.flex_shrink_0
                                        , TwBp.lg
                                            [ Tw.flex_grow_0
                                            ]
                                        ]
                                    ]
                                    [ div
                                        [ Attr.css
                                            [ Tw.flex
                                            , Tw.items_center
                                            , Tw.justify_between
                                            , Tw.w_full
                                            , TwBp.md
                                                [ Tw.w_auto
                                                ]
                                            ]
                                        ]
                                        [ Htmls.a
                                            [ Attr.href "#"
                                            ]
                                            [ Htmls.span
                                                [ Attr.css
                                                    [ Tw.sr_only
                                                    ]
                                                ]
                                                [ Htmls.text "Workflow" ]
                                            , Htmls.img
                                                [ Attr.css
                                                    [ Tw.h_8
                                                    , Tw.w_auto
                                                    , TwBp.sm
                                                        [ Tw.h_10
                                                        ]
                                                    ]
                                                , Attr.src "https://tailwindui.com/img/logos/workflow-mark-indigo-600.svg"
                                                ]
                                                []
                                            ]
                                        , div
                                            [ Attr.css
                                                [ Tw.neg_mr_2
                                                , Tw.flex
                                                , Tw.items_center
                                                , TwBp.md
                                                    [ Tw.hidden
                                                    ]
                                                ]
                                            ]
                                            [ Htmls.button
                                                [ Attr.type_ "button"
                                                , Attr.css
                                                    [ Tw.bg_white
                                                    , Tw.rounded_md
                                                    , Tw.p_2
                                                    , Tw.inline_flex
                                                    , Tw.items_center
                                                    , Tw.justify_center
                                                    , Tw.text_gray_400
                                                    , Css.focus
                                                        [ Tw.outline_none
                                                        , Tw.ring_2
                                                        , Tw.ring_inset
                                                        , Tw.ring_indigo_500
                                                        ]
                                                    , Css.hover
                                                        [ Tw.text_gray_500
                                                        , Tw.bg_gray_100
                                                        ]
                                                    ]
                                                , Attr.attribute "aria-expanded" "false"
                                                ]
                                                [ Htmls.span
                                                    [ Attr.css
                                                        [ Tw.sr_only
                                                        ]
                                                    ]
                                                    [ text "Open main menu" ]
                                                , {- Heroicon name: outline/menu -}
                                                  svg
                                                    [ SvgAttr.css
                                                        [ Tw.h_6
                                                        , Tw.w_6
                                                        ]
                                                    , SvgAttr.fill "none"
                                                    , SvgAttr.viewBox "0 0 24 24"
                                                    , SvgAttr.stroke "currentColor"
                                                    , Attr.attribute "aria-hidden" "true"
                                                    ]
                                                    [ path
                                                        [ SvgAttr.strokeLinecap "round"
                                                        , SvgAttr.strokeLinejoin "round"
                                                        , SvgAttr.strokeWidth "2"
                                                        , SvgAttr.d "M4 6h16M4 12h16M4 18h16"
                                                        ]
                                                        []
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                , div
                                    [ Attr.css
                                        [ Tw.hidden
                                        , TwBp.md
                                            [ Tw.block
                                            , Tw.ml_10
                                            , Tw.pr_4
                                            , Tw.space_x_8
                                            ]
                                        ]
                                    ]
                                    [ Htmls.a
                                        [ Attr.href "#"
                                        , Attr.css
                                            [ Tw.font_medium
                                            , Tw.text_gray_500
                                            , Css.hover
                                                [ Tw.text_gray_900
                                                ]
                                            ]
                                        ]
                                        [ text "Product" ]
                                    , Htmls.a
                                        [ Attr.href "#"
                                        , Attr.css
                                            [ Tw.font_medium
                                            , Tw.text_gray_500
                                            , Css.hover
                                                [ Tw.text_gray_900
                                                ]
                                            ]
                                        ]
                                        [ text "Features" ]
                                    , Htmls.a
                                        [ Attr.href "#"
                                        , Attr.css
                                            [ Tw.font_medium
                                            , Tw.text_gray_500
                                            , Css.hover
                                                [ Tw.text_gray_900
                                                ]
                                            ]
                                        ]
                                        [ text "Marketplace" ]
                                    , Htmls.a
                                        [ Attr.href "#"
                                        , Attr.css
                                            [ Tw.font_medium
                                            , Tw.text_gray_500
                                            , Css.hover
                                                [ Tw.text_gray_900
                                                ]
                                            ]
                                        ]
                                        [ text "Company" ]
                                    , Htmls.a
                                        [ Attr.href "#"
                                        , Attr.css
                                            [ Tw.font_medium
                                            , Tw.text_indigo_600
                                            , Css.hover
                                                [ Tw.text_indigo_500
                                                ]
                                            ]
                                        ]
                                        [ text "Log in" ]
                                    ]
                                ]
                            ]
                        , {-
                             Mobile menu, show/hide based on menu open state.

                             Entering: "duration-150 ease-out"
                               From: "opacity-0 scale-95"
                               To: "opacity-100 scale-100"
                             Leaving: "duration-100 ease-in"
                               From: "opacity-100 scale-100"
                               To: "opacity-0 scale-95"
                          -}
                          div
                            [ Attr.css
                                [ Tw.absolute
                                , Tw.top_0
                                , Tw.inset_x_0
                                , Tw.p_2
                                , Tw.transition
                                , Tw.transform
                                , Tw.origin_top_right
                                , TwBp.md
                                    [ Tw.hidden
                                    ]
                                ]
                            ]
                            [ div
                                [ Attr.css
                                    [ Tw.rounded_lg
                                    , Tw.shadow_md
                                    , Tw.bg_white
                                    , Tw.ring_1
                                    , Tw.ring_black
                                    , Tw.ring_opacity_5
                                    , Tw.overflow_hidden
                                    ]
                                ]
                                [ div
                                    [ Attr.css
                                        [ Tw.px_5
                                        , Tw.pt_4
                                        , Tw.flex
                                        , Tw.items_center
                                        , Tw.justify_between
                                        ]
                                    ]
                                    [ div []
                                        [ Htmls.img
                                            [ Attr.css
                                                [ Tw.h_8
                                                , Tw.w_auto
                                                ]
                                            , Attr.src "https://tailwindui.com/img/logos/workflow-mark-indigo-600.svg"
                                            , Attr.alt ""
                                            ]
                                            []
                                        ]
                                    , div
                                        [ Attr.css
                                            [ Tw.neg_mr_2
                                            ]
                                        ]
                                        [ Htmls.button
                                            [ Attr.type_ "button"
                                            , Attr.css
                                                [ Tw.bg_white
                                                , Tw.rounded_md
                                                , Tw.p_2
                                                , Tw.inline_flex
                                                , Tw.items_center
                                                , Tw.justify_center
                                                , Tw.text_gray_400
                                                , Css.focus
                                                    [ Tw.outline_none
                                                    , Tw.ring_2
                                                    , Tw.ring_inset
                                                    , Tw.ring_indigo_500
                                                    ]
                                                , Css.hover
                                                    [ Tw.text_gray_500
                                                    , Tw.bg_gray_100
                                                    ]
                                                ]
                                            ]
                                            [ Htmls.span
                                                [ Attr.css
                                                    [ Tw.sr_only
                                                    ]
                                                ]
                                                [ text "Close main menu" ]
                                            , {- Heroicon name: outline/x -}
                                              svg
                                                [ SvgAttr.css
                                                    [ Tw.h_6
                                                    , Tw.w_6
                                                    ]
                                                , SvgAttr.fill "none"
                                                , SvgAttr.viewBox "0 0 24 24"
                                                , SvgAttr.stroke "currentColor"
                                                , Attr.attribute "aria-hidden" "true"
                                                ]
                                                [ path
                                                    [ SvgAttr.strokeLinecap "round"
                                                    , SvgAttr.strokeLinejoin "round"
                                                    , SvgAttr.strokeWidth "2"
                                                    , SvgAttr.d "M6 18L18 6M6 6l12 12"
                                                    ]
                                                    []
                                                ]
                                            ]
                                        ]
                                    ]
                                , div
                                    [ Attr.css
                                        [ Tw.px_2
                                        , Tw.pt_2
                                        , Tw.pb_3
                                        , Tw.space_y_1
                                        ]
                                    ]
                                    [ Htmls.a
                                        [ Attr.href "#"
                                        , Attr.css
                                            [ Tw.block
                                            , Tw.px_3
                                            , Tw.py_2
                                            , Tw.rounded_md
                                            , Tw.text_base
                                            , Tw.font_medium
                                            , Tw.text_gray_700
                                            , Css.hover
                                                [ Tw.text_gray_900
                                                , Tw.bg_gray_50
                                                ]
                                            ]
                                        ]
                                        [ text "Product" ]
                                    , Htmls.a
                                        [ Attr.href "#"
                                        , Attr.css
                                            [ Tw.block
                                            , Tw.px_3
                                            , Tw.py_2
                                            , Tw.rounded_md
                                            , Tw.text_base
                                            , Tw.font_medium
                                            , Tw.text_gray_700
                                            , Css.hover
                                                [ Tw.text_gray_900
                                                , Tw.bg_gray_50
                                                ]
                                            ]
                                        ]
                                        [ text "Features" ]
                                    , Htmls.a
                                        [ Attr.href "#"
                                        , Attr.css
                                            [ Tw.block
                                            , Tw.px_3
                                            , Tw.py_2
                                            , Tw.rounded_md
                                            , Tw.text_base
                                            , Tw.font_medium
                                            , Tw.text_gray_700
                                            , Css.hover
                                                [ Tw.text_gray_900
                                                , Tw.bg_gray_50
                                                ]
                                            ]
                                        ]
                                        [ text "Marketplace" ]
                                    , Htmls.a
                                        [ Attr.href "#"
                                        , Attr.css
                                            [ Tw.block
                                            , Tw.px_3
                                            , Tw.py_2
                                            , Tw.rounded_md
                                            , Tw.text_base
                                            , Tw.font_medium
                                            , Tw.text_gray_700
                                            , Css.hover
                                                [ Tw.text_gray_900
                                                , Tw.bg_gray_50
                                                ]
                                            ]
                                        ]
                                        [ text "Company" ]
                                    ]
                                , Htmls.a
                                    [ Attr.href "#"
                                    , Attr.css
                                        [ Tw.block
                                        , Tw.w_full
                                        , Tw.px_5
                                        , Tw.py_3
                                        , Tw.text_center
                                        , Tw.font_medium
                                        , Tw.text_indigo_600
                                        , Tw.bg_gray_50
                                        , Css.hover
                                            [ Tw.bg_gray_100
                                            ]
                                        ]
                                    ]
                                    [ text "Log in" ]
                                ]
                            ]
                        , Htmls.main_
                            [ Attr.css
                                [ Tw.mt_10
                                , Tw.mx_auto
                                , Tw.max_w_7xl
                                , Tw.px_4
                                , TwBp.lg
                                    [ Tw.mt_20
                                    , Tw.px_8
                                    ]
                                , TwBp.md
                                    [ Tw.mt_16
                                    ]
                                , TwBp.sm
                                    [ Tw.mt_12
                                    , Tw.px_6
                                    ]
                                , TwBp.xl
                                    [ Tw.mt_28
                                    ]
                                ]
                            ]
                            [ div
                                [ Attr.css
                                    [ TwBp.lg
                                        [ Tw.text_left
                                        ]
                                    , TwBp.sm
                                        [ Tw.text_center
                                        ]
                                    ]
                                ]
                                [ Htmls.h1
                                    [ Attr.css
                                        [ Tw.text_4xl
                                        , Tw.tracking_tight
                                        , Tw.font_extrabold
                                        , Tw.text_gray_900
                                        , TwBp.md
                                            [ Tw.text_6xl
                                            ]
                                        , TwBp.sm
                                            [ Tw.text_5xl
                                            ]
                                        ]
                                    ]
                                    [ Htmls.span
                                        [ Attr.css
                                            [ Tw.block
                                            , TwBp.xl
                                                [ Tw.inline
                                                -- , Tw.font_serif
                                                ]
                                            ]
                                        ]
                                        [ text "Data to enrich your" ]
                                    , Htmls.span
                                        [ Attr.css
                                            [ Tw.block
                                            , Tw.text_indigo_600
                                            , TwBp.xl
                                                [ Tw.inline
                                                ]
                                            ]
                                        ]
                                        [ text "online business" ]
                                    ]
                                , Htmls.p
                                    [ Attr.css
                                        [ Tw.mt_3
                                        , Tw.text_base
                                        , Tw.text_gray_500
                                        , TwBp.lg
                                            [ Tw.mx_0
                                            ]
                                        , TwBp.md
                                            [ Tw.mt_5
                                            , Tw.text_xl
                                            ]
                                        , TwBp.sm
                                            [ Tw.mt_5
                                            , Tw.text_lg
                                            , Tw.max_w_xl
                                            , Tw.mx_auto
                                            ]
                                        ]
                                    ]
                                    [ text "Anim aute id magna aliqua ad ad non deserunt sunt. Qui irure qui lorem cupidatat commodo. Elit sunt amet fugiat veniam occaecat fugiat aliqua." ]
                                , div
                                    [ Attr.css
                                        [ Tw.mt_5
                                        , TwBp.lg
                                            [ Tw.justify_start
                                            ]
                                        , TwBp.sm
                                            [ Tw.mt_8
                                            , Tw.flex
                                            , Tw.justify_center
                                            ]
                                        ]
                                    ]
                                    [ div
                                        [ Attr.css
                                            [ Tw.rounded_md
                                            , Tw.shadow
                                            ]
                                        ]
                                        [ Htmls.a
                                            [ Attr.href "#"
                                            , Attr.css
                                                [ Tw.w_full
                                                , Tw.flex
                                                , Tw.items_center
                                                , Tw.justify_center
                                                , Tw.px_8
                                                , Tw.py_3
                                                , Tw.border
                                                , Tw.border_transparent
                                                , Tw.text_base
                                                , Tw.font_medium
                                                , Tw.rounded_md
                                                , Tw.text_white
                                                , Tw.bg_indigo_600
                                                , Css.hover
                                                    [ Tw.bg_indigo_700
                                                    ]
                                                , TwBp.md
                                                    [ Tw.py_4
                                                    , Tw.text_lg
                                                    , Tw.px_10
                                                    ]
                                                ]
                                            ]
                                            [ text "Get started" ]
                                        ]
                                    , div
                                        [ Attr.css
                                            [ Tw.mt_3
                                            , TwBp.sm
                                                [ Tw.mt_0
                                                , Tw.ml_3
                                                ]
                                            ]
                                        ]
                                        [ Htmls.a
                                            [ Attr.href "#"
                                            , Attr.css
                                                [ Tw.w_full
                                                , Tw.flex
                                                , Tw.items_center
                                                , Tw.justify_center
                                                , Tw.px_8
                                                , Tw.py_3
                                                , Tw.border
                                                , Tw.border_transparent
                                                , Tw.text_base
                                                , Tw.font_medium
                                                , Tw.rounded_md
                                                , Tw.text_indigo_700
                                                , Tw.bg_indigo_100
                                                , Css.hover
                                                    [ Tw.bg_indigo_200
                                                    ]
                                                , TwBp.md
                                                    [ Tw.py_4
                                                    , Tw.text_lg
                                                    , Tw.px_10
                                                    ]
                                                ]
                                            ]
                                            [ text "Live demo" ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , div
                    [ Attr.css
                        [ TwBp.lg
                            [ Tw.absolute
                            , Tw.inset_y_0
                            , Tw.right_0
                            , Tw.w_1over2
                            ]
                        ]
                    ]
                    [ Htmls.img
                        [ Attr.css
                            [ Tw.h_56
                            , Tw.w_full
                            , Tw.object_contain
                            , TwBp.lg
                                [ Tw.w_full
                                , Tw.h_full
                                ]
                            , TwBp.md
                                [ Tw.h_96
                                ]
                            , TwBp.sm
                                [ Tw.h_72
                                ]
                            ]
                        , Attr.src <| Cloudinary.url "v1619940728/dreamstime_m_212676707_e7mfgt.jpg" "c_crop,h_1250,q_auto:best,w_1550,x_500,y_30,f_auto,dpr_auto"
                        , Attr.alt "Vive tranquilo con Panel SOolar, que al menos el costo de energía esta resuelto."
                        ]
                        []
                    ]

                ]
