module Shared exposing (Model, Msg(..), PageView, RenderedBody, SharedMsg(..), StaticData, UsuarioStatus(..), template)

import Css
import Css.Global
import Html exposing (Html)
import Html.Attributes.Aria as Aria
import Html.Styled as Htmls exposing (div)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import MarkdownRenderer
import OptimizedDecoder as D
import Pages exposing (pages)
import Pages.Directory as Directory exposing (Directory)
import Pages.ImagePath as ImagePath
import Pages.PagePath as PagePath exposing (PagePath)
import Pages.Secrets as Secrets
import Pages.StaticHttp as StaticHttp
import Svg exposing (path, svg)
import Svg.Attributes
import Tailwind.Breakpoints as TwBp
import Tailwind.Utilities as Tw
import TemplateMetadata
import TemplateType exposing (TemplateType)


type alias SharedTemplate templateDemuxMsg msg1 msg2 =
    { init :
        Maybe
            { path :
                { path : PagePath Pages.PathKey
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : TemplateType
            }
        -> ( Model, Cmd Msg )
    , update : Msg -> Model -> ( Model, Cmd Msg )
    , view :
        StaticData
        ->
            { path : PagePath Pages.PathKey
            , frontmatter : TemplateType
            }
        -> Model
        -> (Msg -> templateDemuxMsg)
        -> PageView templateDemuxMsg
        -> { body : Html templateDemuxMsg, title : String }
    , map : (msg1 -> msg2) -> PageView msg1 -> PageView msg2
    , staticData : List ( PagePath Pages.PathKey, TemplateType ) -> StaticHttp.Request StaticData
    , subscriptions : TemplateType -> PagePath Pages.PathKey -> Model -> Sub Msg
    , onPageChange :
        Maybe
            ({ path : PagePath Pages.PathKey
             , query : Maybe String
             , fragment : Maybe String
             }
             -> Msg
            )
    }


template : SharedTemplate msg msg1 msg2
template =
    { init = init
    , update = update
    , view = view
    , map = map
    , staticData = staticData
    , subscriptions = subscriptions
    , onPageChange = Just OnPageChange
    }


type alias RenderedBody =
    ( MarkdownRenderer.TableOfContents, List (Html Never) )


type alias PageView msg =
    { title : String, body : List (Html msg) }


type Msg
    = OnPageChange
        { path : PagePath Pages.PathKey
        , query : Maybe String
        , fragment : Maybe String
        }
    | ToggleMobileMenu
    | ToggleProfileMenu
    | Increment
    | SharedMsg SharedMsg


type alias StaticData =
    Int


type UsuarioStatus
    = Desconocido
    | Rechazado
    | Conocido
    | DiceQueRegresa


type SharedMsg
    = IncrementFromChild
    | User UsuarioStatus


type alias Model =
    { showMobileMenu : Bool
    , counter : Int
    , showProfileMenu : Bool
    , usuario : UsuarioStatus
    }


map : (msg1 -> msg2) -> PageView msg1 -> PageView msg2
map fn doc =
    { title = doc.title
    , body = List.map (Html.map fn) doc.body
    }


init :
    Maybe
        { path :
            { path : PagePath Pages.PathKey
            , query : Maybe String
            , fragment : Maybe String
            }
        , metadata : TemplateType
        }
    -> ( Model, Cmd Msg )
init maybePagePath =
    ( { showMobileMenu = False
      , counter = 0
      , showProfileMenu = False
      , usuario = Desconocido
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnPageChange page ->
            ( { model | showMobileMenu = False }, Cmd.none )

        ToggleMobileMenu ->
            ( { model | showMobileMenu = not model.showMobileMenu }, Cmd.none )

        ToggleProfileMenu ->
            ( { model | showProfileMenu = not model.showProfileMenu }, Cmd.none )

        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        SharedMsg globalMsg ->
            case globalMsg of
                IncrementFromChild ->
                    ( { model | counter = model.counter + 1 }, Cmd.none )

                User status ->
                    case status of
                        Desconocido ->
                            ( { model | usuario = status }, Cmd.none )

                        Rechazado ->
                            ( { model | usuario = status }, Cmd.none )

                        Conocido ->
                            ( { model | usuario = status }, Cmd.none )

                        DiceQueRegresa ->
                            ( { model | usuario = status }, Cmd.none )


subscriptions : TemplateType -> PagePath Pages.PathKey -> Model -> Sub Msg
subscriptions _ _ _ =
    Sub.none


staticData : a -> StaticHttp.Request StaticData
staticData siteMetadata =
    StaticHttp.succeed 12


getRequest : StaticHttp.Request Int
getRequest =
    StaticHttp.get
        (Secrets.succeed "https://api.github.com/repos/dillonkearns/elm-pages")
        (D.field "stargazers_count" D.int)


view :
    StaticData
    ->
        { path : PagePath Pages.PathKey
        , frontmatter : TemplateType
        }
    -> Model
    -> (Msg -> msg)
    -> PageView msg
    -> { body : Html msg, title : String }
view stars page model toMsg pageView =
    { body =
        div
            []
            [ Css.Global.global Tw.globalStyles
            , myNav model page
                |> Htmls.map toMsg
            {-, Htmls.header
                [ Attr.css
                    [ Tw.bg_white
                    , Tw.shadow_sm
                    ]
                ]
                [ div
                    [ Attr.css
                        [ Tw.max_w_7xl
                        , Tw.mx_auto
                        , Tw.py_4
                        , Tw.px_4
                        , TwBp.lg [ Tw.px_8 ]
                        , TwBp.sm [ Tw.px_6 ]
                        ]
                    ]
                    [ Htmls.h1
                        [ Attr.css
                            [ Tw.text_lg
                            , TwBp.lg [ Tw.text_2xl ]
                            , Tw.leading_6
                            , Tw.font_bold
                            , Tw.text_gray_800
                            ]
                        ]
                        [ Htmls.text pageView.title ]
                    ]
                ]
            -}
            , Htmls.main_
                []
                ((incrementView model |> Htmls.map toMsg)
                    :: (pageView.body
                            |> List.map Htmls.fromUnstyled
                       )
                )
            , viewFooter
            ]
            |> Htmls.toUnstyled
    , title = pageView.title
    }


incrementView model =
    div [ Events.onClick Increment ]
        [ Htmls.text <| "Shared count: " ++ String.fromInt model.counter ]


myNav :
    Model
    ->
        { path : PagePath Pages.PathKey
        , frontmatter : TemplateType
        }
    -> Htmls.Html Msg
myNav modelo page =
    let
        getMenu : List TemplateMetadata.Liga
        getMenu =
            case page.frontmatter of
                TemplateType.Page { menu } ->
                    menu

                TemplateType.BlogPost { menu } ->
                    menu

                TemplateType.BlogIndex { menu } ->
                    menu

                TemplateType.Home { menu } ->
                    menu

                TemplateType.SelComp1 { menu } ->
                    menu

                TemplateType.Question { menu } ->
                    menu

        myLogoAndLinks : Htmls.Html msg
        myLogoAndLinks =
            div
                [ Attr.css [ Tw.flex, Tw.items_center ] ]
                [ div
                    -- EL LOGO
                    [ Attr.css [ Tw.flex_shrink_0 ] ]
                    [ Htmls.img
                        [ Attr.css [ Tw.h_8, Tw.w_8 ]
                        , Attr.src "https://tailwindui.com/img/logos/workflow-mark-indigo-500.svg"
                        , Attr.alt "Workflow"
                        ]
                        []
                    ]
                , div
                    -- LIGAS DE NAVEGACION
                    [ Attr.css [ Tw.hidden, TwBp.md [ Tw.block ] ] ]
                    [ div
                        [ Attr.css
                            [ Tw.ml_10
                            , Tw.flex
                            , Tw.items_baseline
                            , Tw.space_x_4
                            ]
                        ]
                        (ligasChulas False <| getMenu)
                    ]
                ]

        myHiddenBlock : Htmls.Html Msg
        myHiddenBlock =
            div
                [ Attr.css
                    [ Tw.hidden
                    , TwBp.md [ Tw.block ]
                    ]
                ]
                [ div
                    [ Attr.css
                        [ Tw.ml_4
                        , Tw.flex
                        , Tw.items_center
                        , TwBp.md [ Tw.ml_6 ]
                        ]
                    ]
                    [ div
                        -- Profile dropdown --
                        [ Attr.css
                            [ Tw.ml_3
                            , Tw.relative
                            ]
                        ]
                        [ div
                            []
                            [ Htmls.button
                                [ Attr.css
                                    [ Tw.max_w_xs
                                    , Tw.bg_gray_800
                                    , Tw.rounded_full
                                    , Tw.flex
                                    , Tw.items_center
                                    , Tw.text_sm
                                    , Tw.text_white
                                    , Css.focus
                                        [ Tw.outline_none
                                        , Tw.ring_2
                                        , Tw.ring_offset_2
                                        , Tw.ring_offset_gray_800
                                        , Tw.ring_white
                                        ]
                                    ]
                                , Attr.id "user-menu"
                                , Aria.ariaHasPopup "true" |> Attr.fromUnstyled
                                , Events.onClick ToggleProfileMenu
                                ]
                                [ Htmls.span
                                    [ Attr.css [ Tw.sr_only ] ]
                                    [ Htmls.text "Open user menu" ]
                                , Htmls.img
                                    [ Attr.css
                                        [ Tw.h_8
                                        , Tw.w_8
                                        , Tw.rounded_full
                                        ]
                                    , Attr.src "https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?ixlib=rb-1.2.1&ixqx=g09zpRVLoT&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80"
                                    , Attr.alt ""
                                    ]
                                    []
                                ]
                            ]
                        , if modelo.showProfileMenu then
                            div
                                [ Attr.css
                                    [ Tw.origin_top_right
                                    , Tw.absolute
                                    , Tw.right_0
                                    , Tw.mt_2
                                    , Tw.w_48
                                    , Tw.rounded_md
                                    , Tw.shadow_lg
                                    , Tw.py_1
                                    , Tw.bg_white
                                    , Tw.ring_1
                                    , Tw.ring_black
                                    , Tw.ring_opacity_5
                                    ]
                                , Aria.role "menu" |> Attr.fromUnstyled

                                -- , Aria.aria-orientation "vertical"
                                , Aria.ariaLabelledby "user-menu" |> Attr.fromUnstyled
                                ]
                                (menuItems
                                    [ Tw.block
                                    , Tw.px_4
                                    , Tw.py_2
                                    , Tw.text_sm
                                    , Tw.text_gray_700
                                    , Css.hover [ Tw.bg_gray_100 ]
                                    ]
                                )

                          else
                            Htmls.i [] []
                        ]
                    ]
                ]

        myHiddenMenu : Htmls.Html msg
        myHiddenMenu =
            div
                [ Attr.css <|
                    TwBp.md [ Tw.hidden ]
                        :: (if modelo.showMobileMenu then
                                [ Tw.block ]

                            else
                                [ Tw.hidden ]
                           )
                ]
                [ div
                    [ Attr.css
                        [ Tw.px_2
                        , Tw.pt_2
                        , Tw.pb_3
                        , Tw.space_y_1
                        , TwBp.sm [ Tw.px_3 ]
                        ]
                    ]
                    (ligasChulas True <| getMenu)
                , div
                    [ Attr.css
                        [ Tw.pt_4
                        , Tw.pb_3
                        , Tw.border_t
                        , Tw.border_gray_700
                        ]
                    ]
                    [ div
                        [ Attr.css
                            [ Tw.flex
                            , Tw.items_center
                            , Tw.px_5
                            ]
                        ]
                        [ div
                            [ Attr.css [ Tw.flex_shrink_0 ] ]
                            [ Htmls.img
                                [ Attr.css [ Tw.h_10, Tw.w_10, Tw.rounded_full ]
                                , Attr.src "https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?ixlib=rb-1.2.1&ixqx=g09zpRVLoT&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80"
                                , Attr.alt ""
                                ]
                                []
                            , div
                                [ Attr.css [ Tw.ml_3 ] ]
                                [ div
                                    [ Attr.css [ Tw.text_base, Tw.font_medium, Tw.text_white ] ]
                                    [ Htmls.text "Tom Cook" ]
                                , div
                                    [ Attr.css [ Tw.text_sm, Tw.font_medium, Tw.text_gray_400 ] ]
                                    [ Htmls.text "tom@example.com" ]
                                ]
                            ]
                        ]
                    , div [ Attr.css [ Tw.mt_3, Tw.px_2, Tw.space_y_1 ] ]
                        (menuItems
                            [ Tw.block
                            , Tw.px_3
                            , Tw.py_2
                            , Tw.rounded_md
                            , Tw.text_base
                            , Tw.font_medium
                            , Tw.text_gray_400
                            , Css.hover [ Tw.text_white, Tw.bg_gray_700 ]
                            ]
                        )
                    ]
                ]

        ligasChulas :
            Bool
            -> List TemplateMetadata.Liga
            -> List (Htmls.Html msg)
        ligasChulas esMovil menus =
            let
                clasesBase =
                    [ Tw.text_gray_300
                    , Css.hover [ Tw.bg_gray_700, Tw.text_white ]
                    , Tw.px_3
                    , Tw.py_2
                    , Tw.rounded_md
                    , Tw.font_medium
                    ]

                claseActual =
                    [ Tw.bg_gray_900
                    , Tw.text_white
                    , Tw.px_3
                    , Tw.py_2
                    , Tw.rounded_md
                    , Tw.font_medium
                    ]

                claseActualEnMovil =
                    [ Tw.block, Tw.text_base ]

                claseActualEnDesktop =
                    [ Tw.text_sm ]

                claseExtraPaMovil =
                    [ Tw.block, Tw.text_base ]

                claseExtraPaDesktop =
                    [ Tw.text_sm ]

                clasesQueVan esParaMovil liga =
                    if PagePath.toString page.path == liga.direccion then
                        case esParaMovil of
                            True ->
                                claseActual ++ claseActualEnMovil

                            False ->
                                claseActual ++ claseActualEnDesktop

                    else
                        case esParaMovil of
                            True ->
                                clasesBase ++ claseExtraPaMovil

                            False ->
                                clasesBase ++ claseExtraPaDesktop

                -- ligaChula : List Tw.Styles -> TemplateMetadata.Liga -> Html msg
                ligaChula clases liga =
                    Htmls.a
                        [ Attr.href liga.direccion
                        , Attr.css clases
                        ]
                        [ Htmls.text liga.queDice ]
            in
            List.map
                (\algoDelMenu ->
                    ligaChula
                        (clasesQueVan esMovil algoDelMenu)
                        algoDelMenu
                )
                menus

        -- menuItems : List Styles -> List (Html msg)
        menuItems clases =
            [ Htmls.a
                [ Attr.href "#"
                , Attr.css clases
                , Aria.role "menuitem" |> Attr.fromUnstyled
                ]
                [ Htmls.text "Your Profile" ]
            , Htmls.a
                [ Attr.css clases
                , Aria.role "menuitem" |> Attr.fromUnstyled
                ]
                [ Htmls.text "Settings" ]
            , Htmls.a
                [ Attr.css clases
                , Aria.role "menuitem" |> Attr.fromUnstyled
                ]
                [ Htmls.text "Sign out" ]
            ]

        -- heroiconOutlineMenu : Html msg
        heroiconOutlineMenu =
            div
                [ Attr.css
                    [ Tw.h_6, Tw.w_6, Tw.block ]
                ]
                [ svg
                    -- [ Svg.Attributes.Attr.css [h-6 w-6 block"
                    -- , xmlns="http://www.w3.org/2000/svg"
                    [ Svg.Attributes.fill "none"
                    , Svg.Attributes.viewBox "0 0 24 24"
                    , Svg.Attributes.stroke "currentColor"

                    -- aria-hidden="true"
                    ]
                    [ Svg.path
                        [ Svg.Attributes.strokeLinecap "round"
                        , Svg.Attributes.strokeLinejoin "round"
                        , Svg.Attributes.strokeWidth "2"
                        , Svg.Attributes.d "M4 6h16M4 12h16M4 18h16"
                        ]
                        []
                    ]
                    |> Htmls.fromUnstyled
                ]

        heroiconOutlineX : Htmls.Html msg
        heroiconOutlineX =
            div
                [ Attr.css [ Tw.h_6, Tw.w_6, Tw.block ] ]
                [ svg
                    -- Svg.Attributes.Attr.css [h-6 w-6 block"
                    -- xmlns="http://www.w3.org/2000/svg"
                    [ Svg.Attributes.fill "none"
                    , Svg.Attributes.viewBox "0 0 24 24"
                    , Svg.Attributes.stroke "currentColor"

                    -- aria-hidden="true"
                    ]
                    [ path
                        [ Svg.Attributes.strokeLinecap "round"
                        , Svg.Attributes.strokeLinejoin "round"
                        , Svg.Attributes.strokeWidth "2"
                        , Svg.Attributes.d "M6 18L18 6M6 6l12 12"
                        ]
                        []
                    ]
                    |> Htmls.fromUnstyled
                ]

        mobileMenuButton : Htmls.Html Msg
        mobileMenuButton =
            div
                [ Attr.css
                    [ Tw.neg_mr_2
                    , Tw.flex
                    , TwBp.md [ Tw.hidden ]
                    ]
                ]
                [ Htmls.button
                    [ Attr.css
                        [ Tw.bg_gray_800
                        , Tw.inline_flex
                        , Tw.items_center
                        , Tw.justify_center
                        , Tw.p_2
                        , Tw.rounded_md
                        , Tw.text_gray_400
                        , Css.hover
                            [ Tw.text_white
                            , Tw.bg_gray_700
                            ]
                        , Css.focus
                            [ Tw.outline_none
                            , Tw.ring_2
                            , Tw.ring_offset_2
                            , Tw.ring_offset_gray_800
                            , Tw.ring_white
                            ]
                        ]
                    , Events.onClick ToggleMobileMenu
                    ]
                    [ Htmls.span [ Attr.css [ Tw.sr_only ] ] [ Htmls.text "Open main menu" ]
                    , if modelo.showMobileMenu then
                        heroiconOutlineX

                      else
                        heroiconOutlineMenu
                    ]
                ]
    in
    Htmls.nav
        [ Attr.css [ Tw.bg_gray_800 ] ]
        [ div
            [ Attr.css
                [ Tw.max_w_7xl
                , Tw.mx_auto
                , Tw.px_4
                , TwBp.lg [ Tw.px_8 ]
                , TwBp.sm [ Tw.px_6 ]
                ]
            ]
            [ div
                [ Attr.css
                    [ Tw.flex
                    , Tw.items_center
                    , Tw.justify_between
                    , Tw.h_16
                    ]
                ]
                [ myLogoAndLinks
                , myHiddenBlock
                , mobileMenuButton
                ]
            ]
        , myHiddenMenu
        ]


viewFooter : Htmls.Html msg
viewFooter =
    let
        ligaAlPie : String -> String -> Htmls.Html msg
        ligaAlPie liga texto =
            div
                [ Attr.css [ Tw.px_5, Tw.py_2 ] ]
                [ Htmls.a
                    [ Attr.href liga
                    , Attr.css
                        [ Tw.text_base
                        , Tw.text_gray_600
                        , Css.hover [ Tw.text_black ]
                        ]
                    ]
                    [ Htmls.text texto ]
                ]

        svgSocialIcon : String -> Htmls.Html msg
        svgSocialIcon superD =
            div
                [ Attr.css [ Tw.h_6, Tw.w_6 ] ]
                [ svg
                    -- [ Svg.Attributes.Attr.css []
                    [ Svg.Attributes.fill "currentColor"
                    , Svg.Attributes.viewBox "0 0 24 24"

                    -- aria-hidden="true"
                    ]
                    [ Svg.path
                        [ Svg.Attributes.fillRule "evenodd"
                        , Svg.Attributes.d superD
                        , Svg.Attributes.clipRule "evenodd"
                        ]
                        []
                    ]
                    |> Htmls.fromUnstyled
                ]

        svgFacebook =
            svgSocialIcon "M22 12c0-5.523-4.477-10-10-10S2 6.477 2 12c0 4.991 3.657 9.128 8.438 9.878v-6.987h-2.54V12h2.54V9.797c0-2.506 1.492-3.89 3.777-3.89 1.094 0 2.238.195 2.238.195v2.46h-1.26c-1.243 0-1.63.771-1.63 1.562V12h2.773l-.443 2.89h-2.33v6.988C18.343 21.128 22 16.991 22 12z"

        svgGithub =
            svgSocialIcon "M12 2C6.477 2 2 6.484 2 12.017c0 4.425 2.865 8.18 6.839 9.504.5.092.682-.217.682-.483 0-.237-.008-.868-.013-1.703-2.782.605-3.369-1.343-3.369-1.343-.454-1.158-1.11-1.466-1.11-1.466-.908-.62.069-.608.069-.608 1.003.07 1.531 1.032 1.531 1.032.892 1.53 2.341 1.088 2.91.832.092-.647.35-1.088.636-1.338-2.22-.253-4.555-1.113-4.555-4.951 0-1.093.39-1.988 1.029-2.688-.103-.253-.446-1.272.098-2.65 0 0 .84-.27 2.75 1.026A9.564 9.564 0 0112 6.844c.85.004 1.705.115 2.504.337 1.909-1.296 2.747-1.027 2.747-1.027.546 1.379.202 2.398.1 2.651.64.7 1.028 1.595 1.028 2.688 0 3.848-2.339 4.695-4.566 4.943.359.309.678.92.678 1.855 0 1.338-.012 2.419-.012 2.747 0 .268.18.58.688.482A10.019 10.019 0 0022 12.017C22 6.484 17.522 2 12 2z"

        svgTwitter =
            svgSocialIcon "M8.29 20.251c7.547 0 11.675-6.253 11.675-11.675 0-.178 0-.355-.012-.53A8.348 8.348 0 0022 5.92a8.19 8.19 0 01-2.357.646 4.118 4.118 0 001.804-2.27 8.224 8.224 0 01-2.605.996 4.107 4.107 0 00-6.993 3.743 11.65 11.65 0 01-8.457-4.287 4.106 4.106 0 001.27 5.477A4.072 4.072 0 012.8 9.713v.052a4.105 4.105 0 003.292 4.022 4.095 4.095 0 01-1.853.07 4.108 4.108 0 003.834 2.85A8.233 8.233 0 012 18.407a11.616 11.616 0 006.29 1.84"

        svgInstagram =
            svgSocialIcon "M12.315 2c2.43 0 2.784.013 3.808.06 1.064.049 1.791.218 2.427.465a4.902 4.902 0 011.772 1.153 4.902 4.902 0 011.153 1.772c.247.636.416 1.363.465 2.427.048 1.067.06 1.407.06 4.123v.08c0 2.643-.012 2.987-.06 4.043-.049 1.064-.218 1.791-.465 2.427a4.902 4.902 0 01-1.153 1.772 4.902 4.902 0 01-1.772 1.153c-.636.247-1.363.416-2.427.465-1.067.048-1.407.06-4.123.06h-.08c-2.643 0-2.987-.012-4.043-.06-1.064-.049-1.791-.218-2.427-.465a4.902 4.902 0 01-1.772-1.153 4.902 4.902 0 01-1.153-1.772c-.247-.636-.416-1.363-.465-2.427-.047-1.024-.06-1.379-.06-3.808v-.63c0-2.43.013-2.784.06-3.808.049-1.064.218-1.791.465-2.427a4.902 4.902 0 011.153-1.772A4.902 4.902 0 015.45 2.525c.636-.247 1.363-.416 2.427-.465C8.901 2.013 9.256 2 11.685 2h.63zm-.081 1.802h-.468c-2.456 0-2.784.011-3.807.058-.975.045-1.504.207-1.857.344-.467.182-.8.398-1.15.748-.35.35-.566.683-.748 1.15-.137.353-.3.882-.344 1.857-.047 1.023-.058 1.351-.058 3.807v.468c0 2.456.011 2.784.058 3.807.045.975.207 1.504.344 1.857.182.466.399.8.748 1.15.35.35.683.566 1.15.748.353.137.882.3 1.857.344 1.054.048 1.37.058 4.041.058h.08c2.597 0 2.917-.01 3.96-.058.976-.045 1.505-.207 1.858-.344.466-.182.8-.398 1.15-.748.35-.35.566-.683.748-1.15.137-.353.3-.882.344-1.857.048-1.055.058-1.37.058-4.041v-.08c0-2.597-.01-2.917-.058-3.96-.045-.976-.207-1.505-.344-1.858a3.097 3.097 0 00-.748-1.15 3.098 3.098 0 00-1.15-.748c-.353-.137-.882-.3-1.857-.344-1.023-.047-1.351-.058-3.807-.058zM12 6.865a5.135 5.135 0 110 10.27 5.135 5.135 0 010-10.27zm0 1.802a3.333 3.333 0 100 6.666 3.333 3.333 0 000-6.666zm5.338-3.205a1.2 1.2 0 110 2.4 1.2 1.2 0 010-2.4z"

        ligaIcono : String -> String -> SocialIcons -> Htmls.Html msg
        ligaIcono direccion srCual iconoSocial =
            Htmls.a
                [ Attr.href direccion
                , Attr.css [ Tw.text_gray_400, Css.hover [ Tw.text_gray_500 ] ]
                ]
                [ Htmls.span
                    [ Attr.css [ Tw.sr_only ] ]
                    [ Htmls.text srCual ]
                , case iconoSocial of
                    Facebook ->
                        svgFacebook

                    Instagram ->
                        svgInstagram

                    Twitter ->
                        svgTwitter

                    Github ->
                        svgGithub
                ]
    in
    Htmls.footer
        []
        [ div
            []
            [ div
                [ Attr.css [ Tw.bg_white, Tw.py_4 ] ]
                []
            , div
                [ Attr.css [ Tw.bg_gray_200 ] ]
                [ div
                    [ Attr.css
                        [ Tw.max_w_7xl
                        , Tw.mx_auto
                        , Tw.py_12
                        , Tw.px_4
                        , Tw.overflow_hidden
                        , TwBp.lg [ Tw.px_8 ]
                        , TwBp.sm [ Tw.px_6 ]
                        ]
                    ]
                    [ Htmls.nav
                        [ Attr.css
                            [ Tw.neg_mx_5
                            , Tw.neg_my_2
                            , Tw.flex
                            , Tw.flex_wrap
                            , Tw.justify_center
                            ]
                        , Aria.ariaLabel "Footer" |> Attr.fromUnstyled
                        ]
                        [ ligaAlPie "#" "About"
                        , ligaAlPie "#" "Blog"
                        , ligaAlPie "#" "Jobs"
                        , ligaAlPie "#" "Press"
                        , ligaAlPie "#" "Accesibility"
                        , ligaAlPie "#" "Partners"
                        ]
                    , div
                        [ Attr.css
                            [ Tw.mt_8
                            , Tw.flex
                            , Tw.justify_center
                            , Tw.space_x_6
                            ]
                        ]
                        [ ligaIcono "facebook.com" "facebook" Facebook
                        , ligaIcono "instagram.com" "Instagram" Instagram
                        , ligaIcono "twitter.com" "Twitter" Twitter
                        , ligaIcono "github.com" "GitHub" Github
                        ]
                    , Htmls.p
                        [ Attr.css
                            [ Tw.mt_8
                            , Tw.text_center
                            , Tw.text_base
                            , Tw.text_gray_500
                            ]
                        ]
                        [ Htmls.text "&copy; 2020 Workflow, Inc. All rights reserved." ]
                    ]
                ]
            ]
        ]


type SocialIcons
    = Facebook
    | Instagram
    | Twitter
    | Github
