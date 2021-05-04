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
import Site
import Svg.Styled as Svg exposing (path, svg)
import Svg.Styled.Attributes as SvgAttr
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
            []
            [ {- div
                         [ Attr.css [ Tw.prose, TwBp.lg [ Tw.prose_xl ] ] ]
                         (Tuple.second rendered
                             |> List.map (Html.map never)
                             |> List.map Htmls.fromUnstyled
                         )
                     ]
                 ,
              -}
              viewAboveTheFold
            ]
            |> Htmls.toUnstyled
        ]
    }


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
                [ Htmls.main_
                    [ Attr.css
                        [ Tw.mt_4
                        , Tw.mx_auto
                        , Tw.max_w_7xl
                        , Tw.px_4
                        , TwBp.lg
                            [ Tw.mt_0
                            , Tw.px_8
                            ]
                        , TwBp.md
                            [ Tw.mt_10
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
                                , Tw.pt_16
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
                                    , Tw.mt_10
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
                                    , Tw.mt_10
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
                    , Tw.object_cover
                    , TwBp.lg
                        [ Tw.w_full
                        , Tw.h_full
                        , Tw.object_scale_down
                        , Tw.object_left
                        ]
                    , TwBp.md
                        [ Tw.h_96
                        ]
                    , TwBp.sm
                        [ Tw.h_72
                        ]
                    ]
                , Attr.src
                     <| Cloudinary.url
                         "v1619940728/dreamstime_m_212676707_e7mfgt.jpg"
                         "q_auto:best,y_30,a_hflip,f_auto,dpr_auto"
                , Attr.alt "Vive tranquilo con Panel Solar, que al menos el costo de energía esta resuelto."
                ]
                []
            ]
        ]
