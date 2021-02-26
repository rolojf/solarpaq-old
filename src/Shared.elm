module Shared exposing (Model, Msg(..), PageView, RenderedBody, SharedMsg(..), StaticData, template, UsuarioStatus(..))

import Browser.Dom as Dom
import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class)
import Html.Attributes.Aria as Aria
import Html.Events
import MarkdownRenderer
import OptimizedDecoder as D
import Pages exposing (pages)
import Pages.Directory as Directory exposing (Directory)
import Pages.ImagePath as ImagePath
import Pages.PagePath as PagePath exposing (PagePath)
import Pages.Secrets as Secrets
import Pages.StaticHttp as StaticHttp
import Palette
import Process
import Svg exposing (path, svg)
import Svg.Attributes
import Task
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
        div []
            [ myNav model page |> Html.map toMsg
            , Html.header
                [ class "bg-white shadow-sm" ]
                [ div
                    [ class "max-w-7xl mx-auto py-4 px-4 sm:px-6 lg:px-8" ]
                    [ Html.h1
                        [ class "text-lg leading-6 font-semibold text-gray-900" ]
                        [ Html.text "Dashboard" ]
                    ]
                ]
            , Html.main_
                []
                [ div
                    [ class "max-w-7xl mx-auto sm:px-6 lg:px-8" ]
                    ((incrementView model
                        |> Html.map toMsg
                     )
                        :: pageView.body
                    )
                ]
            , viewFooter
            ]
    , title = pageView.title
    }


incrementView model =
    div [ Html.Events.onClick Increment ]
        [ Html.text <| "Shared count: " ++ String.fromInt model.counter ]


myNav :
    Model
    ->
        { path : PagePath Pages.PathKey
        , frontmatter : TemplateType
        }
    -> Html Msg
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

                TemplateType.SelComp1 { menu } ->
                    menu

                TemplateType.Home { menu } ->
                                    menu

        myLogoAndLinks : Html msg
        myLogoAndLinks =
            div
                [ class "flex items-center" ]
                [ div
                    -- EL LOGO
                    [ class "flex-shrink-0" ]
                    [ Html.img
                        [ class "h-8 w-8"
                        , Attr.src "https://tailwindui.com/img/logos/workflow-mark-indigo-500.svg"
                        , Attr.alt "Workflow"
                        ]
                        []
                    ]
                , div
                    -- LIGAS DE NAVEGACION
                    [ class "hidden md:block" ]
                    [ div
                        [ class "ml-10 flex items-baseline space-x-4" ]
                        (ligasChulas False <| getMenu)
                    ]
                ]

        myHiddenBlock : Html Msg
        myHiddenBlock =
            div
                [ class "hidden md:block" ]
                [ div
                    [ class "ml-4 flex items-center md:ml-6" ]
                    [ div
                        -- Profile dropdown --
                        [ class "ml-3 relative" ]
                        [ div
                            []
                            [ Html.button
                                [ class "max-w-xs bg-gray-800 rounded-full flex items-center text-sm text-white focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-gray-800 focus:ring-white"
                                , Attr.id "user-menu"
                                , Aria.ariaHasPopup "true"
                                , Html.Events.onClick ToggleProfileMenu
                                ]
                                [ Html.span [ class "sr-only" ] [ Html.text "Open user menu" ]
                                , Html.img
                                    [ class "h-8 w-8 rounded-full"
                                    , Attr.src "https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?ixlib=rb-1.2.1&ixqx=g09zpRVLoT&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80"
                                    , Attr.alt ""
                                    ]
                                    []
                                ]
                            ]
                        , if modelo.showProfileMenu then
                            div
                                [ class "origin-top-right absolute right-0 mt-2 w-48 rounded-md shadow-lg py-1 bg-white ring-1 ring-black ring-opacity-5"
                                , Aria.role "menu"

                                -- , Aria.aria-orientation "vertical"
                                , Aria.ariaLabelledby "user-menu"
                                ]
                                (menuItems "block px-4 py-2 text-sm text-gray-700 hover:bg-gray-100")

                          else
                            Html.i [] []
                        ]
                    ]
                ]

        myHiddenMenu : Html msg
        myHiddenMenu =
            div
                [ class <|
                    "md:hidden "
                        ++ (if modelo.showMobileMenu then
                                "block"

                            else
                                "hidden"
                           )
                ]
                [ div
                    [ class "px-2 pt-2 pb-3 space-y-1 sm:px-3" ]
                    (ligasChulas True <| getMenu)
                , div
                    [ class "pt-4 pb-3 border-t border-gray-700" ]
                    [ div
                        [ class "flex items-center px-5" ]
                        [ div
                            [ class "flex-shrink-0" ]
                            [ Html.img
                                [ class "h-10 w-10 rounded-full"
                                , Attr.src "https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?ixlib=rb-1.2.1&ixqx=g09zpRVLoT&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80"
                                , Attr.alt ""
                                ]
                                []
                            , div
                                [ class "ml-3" ]
                                [ div
                                    [ class "text-base font-medium text-white" ]
                                    [ Html.text "Tom Cook" ]
                                , div
                                    [ class "text-sm font-medium text-gray-400" ]
                                    [ Html.text "tom@example.com" ]
                                ]
                            ]
                        ]
                    , div [ class "mt-3 px-2 space-y-1" ]
                        (menuItems "block px-3 py-2 rounded-md text-base font-medium text-gray-400 hover:text-white hover:bg-gray-700")
                    ]
                ]

        ligasChulas :
            Bool
            -> List TemplateMetadata.Liga
            -> List (Html msg)
        ligasChulas esMovil menus =
            let
                clasesBase =
                    "text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md font-medium"

                claseActual =
                    "bg-gray-900 text-white px-3 py-2 rounded-md font-medium"

                claseActualEnMovil =
                    " block text-base"

                claseActualEnDesktop =
                    " text-sm"

                claseExtraPaMovil =
                    " block text-base"

                claseExtraPaDesktop =
                    " text-sm"

                clasesQueVan :
                    Bool
                    -> TemplateMetadata.Liga
                    -> String
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

                ligaChula : String -> TemplateMetadata.Liga -> Html msg
                ligaChula clases liga =
                    Html.a
                        [ Attr.href liga.direccion
                        , class clases
                        ]
                        [ Html.text liga.queDice ]
            in
            List.map
                (\algoDelMenu ->
                    ligaChula
                        (clasesQueVan esMovil algoDelMenu)
                        algoDelMenu
                )
                menus

        menuItems : String -> List (Html msg)
        menuItems clases =
            [ Html.a
                [ Attr.href "#"
                , class clases
                , Aria.role "menuitem"
                ]
                [ Html.text "Your Profile" ]
            , Html.a
                [ class clases
                , Aria.role "menuitem"
                ]
                [ Html.text "Settings" ]
            , Html.a
                [ class clases
                , Aria.role "menuitem"
                ]
                [ Html.text "Sign out" ]
            ]

        heroiconOutlineMenu : Html msg
        heroiconOutlineMenu =
            svg
                [ Svg.Attributes.class "h-6 w-6 block"

                -- , xmlns="http://www.w3.org/2000/svg"
                , Svg.Attributes.fill "none"
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

        heroiconOutlineX : Html msg
        heroiconOutlineX =
            svg
                [ Svg.Attributes.class "h-6 w-6 block"

                -- xmlns="http://www.w3.org/2000/svg"
                , Svg.Attributes.fill "none"
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

        mobileMenuButton : Html Msg
        mobileMenuButton =
            div
                [ class "-mr-2 flex md:hidden" ]
                [ Html.button
                    [ class "bg-gray-800 inline-flex items-center justify-center p-2 rounded-md text-gray-400 hover:text-white hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-gray-800 focus:ring-white"
                    , Html.Events.onClick ToggleMobileMenu
                    ]
                    [ Html.span [ class "sr-only" ] [ Html.text "Open main menu" ]
                    , if modelo.showMobileMenu then
                        heroiconOutlineX

                      else
                        heroiconOutlineMenu
                    ]
                ]
    in
    Html.nav
        [ class "bg-gray-800" ]
        [ div
            [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
            [ div
                [ class "flex items-center justify-between h-16" ]
                [ myLogoAndLinks
                , myHiddenBlock
                , mobileMenuButton
                ]
            ]
        , myHiddenMenu
        ]


viewFooter : Html msg
viewFooter =
    let
        ligaAlPie : String -> String -> Html msg
        ligaAlPie liga texto =
            div
                [ class "px-5 py-2" ]
                [ Html.a
                    [ Attr.href liga
                    , class "text-base text-gray-600 hover:text-black"
                    ]
                    [ Html.text texto ]
                ]

        svgSocialIcon : String -> Html msg
        svgSocialIcon superD =
            svg
                [ Svg.Attributes.class "h-6 w-6"
                , Svg.Attributes.fill "currentColor"
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

        svgFacebook =
            svgSocialIcon "M22 12c0-5.523-4.477-10-10-10S2 6.477 2 12c0 4.991 3.657 9.128 8.438 9.878v-6.987h-2.54V12h2.54V9.797c0-2.506 1.492-3.89 3.777-3.89 1.094 0 2.238.195 2.238.195v2.46h-1.26c-1.243 0-1.63.771-1.63 1.562V12h2.773l-.443 2.89h-2.33v6.988C18.343 21.128 22 16.991 22 12z"

        svgGithub =
            svgSocialIcon "M12 2C6.477 2 2 6.484 2 12.017c0 4.425 2.865 8.18 6.839 9.504.5.092.682-.217.682-.483 0-.237-.008-.868-.013-1.703-2.782.605-3.369-1.343-3.369-1.343-.454-1.158-1.11-1.466-1.11-1.466-.908-.62.069-.608.069-.608 1.003.07 1.531 1.032 1.531 1.032.892 1.53 2.341 1.088 2.91.832.092-.647.35-1.088.636-1.338-2.22-.253-4.555-1.113-4.555-4.951 0-1.093.39-1.988 1.029-2.688-.103-.253-.446-1.272.098-2.65 0 0 .84-.27 2.75 1.026A9.564 9.564 0 0112 6.844c.85.004 1.705.115 2.504.337 1.909-1.296 2.747-1.027 2.747-1.027.546 1.379.202 2.398.1 2.651.64.7 1.028 1.595 1.028 2.688 0 3.848-2.339 4.695-4.566 4.943.359.309.678.92.678 1.855 0 1.338-.012 2.419-.012 2.747 0 .268.18.58.688.482A10.019 10.019 0 0022 12.017C22 6.484 17.522 2 12 2z"

        svgTwitter =
            svgSocialIcon "M8.29 20.251c7.547 0 11.675-6.253 11.675-11.675 0-.178 0-.355-.012-.53A8.348 8.348 0 0022 5.92a8.19 8.19 0 01-2.357.646 4.118 4.118 0 001.804-2.27 8.224 8.224 0 01-2.605.996 4.107 4.107 0 00-6.993 3.743 11.65 11.65 0 01-8.457-4.287 4.106 4.106 0 001.27 5.477A4.072 4.072 0 012.8 9.713v.052a4.105 4.105 0 003.292 4.022 4.095 4.095 0 01-1.853.07 4.108 4.108 0 003.834 2.85A8.233 8.233 0 012 18.407a11.616 11.616 0 006.29 1.84"

        svgInstagram =
            svgSocialIcon "M12.315 2c2.43 0 2.784.013 3.808.06 1.064.049 1.791.218 2.427.465a4.902 4.902 0 011.772 1.153 4.902 4.902 0 011.153 1.772c.247.636.416 1.363.465 2.427.048 1.067.06 1.407.06 4.123v.08c0 2.643-.012 2.987-.06 4.043-.049 1.064-.218 1.791-.465 2.427a4.902 4.902 0 01-1.153 1.772 4.902 4.902 0 01-1.772 1.153c-.636.247-1.363.416-2.427.465-1.067.048-1.407.06-4.123.06h-.08c-2.643 0-2.987-.012-4.043-.06-1.064-.049-1.791-.218-2.427-.465a4.902 4.902 0 01-1.772-1.153 4.902 4.902 0 01-1.153-1.772c-.247-.636-.416-1.363-.465-2.427-.047-1.024-.06-1.379-.06-3.808v-.63c0-2.43.013-2.784.06-3.808.049-1.064.218-1.791.465-2.427a4.902 4.902 0 011.153-1.772A4.902 4.902 0 015.45 2.525c.636-.247 1.363-.416 2.427-.465C8.901 2.013 9.256 2 11.685 2h.63zm-.081 1.802h-.468c-2.456 0-2.784.011-3.807.058-.975.045-1.504.207-1.857.344-.467.182-.8.398-1.15.748-.35.35-.566.683-.748 1.15-.137.353-.3.882-.344 1.857-.047 1.023-.058 1.351-.058 3.807v.468c0 2.456.011 2.784.058 3.807.045.975.207 1.504.344 1.857.182.466.399.8.748 1.15.35.35.683.566 1.15.748.353.137.882.3 1.857.344 1.054.048 1.37.058 4.041.058h.08c2.597 0 2.917-.01 3.96-.058.976-.045 1.505-.207 1.858-.344.466-.182.8-.398 1.15-.748.35-.35.566-.683.748-1.15.137-.353.3-.882.344-1.857.048-1.055.058-1.37.058-4.041v-.08c0-2.597-.01-2.917-.058-3.96-.045-.976-.207-1.505-.344-1.858a3.097 3.097 0 00-.748-1.15 3.098 3.098 0 00-1.15-.748c-.353-.137-.882-.3-1.857-.344-1.023-.047-1.351-.058-3.807-.058zM12 6.865a5.135 5.135 0 110 10.27 5.135 5.135 0 010-10.27zm0 1.802a3.333 3.333 0 100 6.666 3.333 3.333 0 000-6.666zm5.338-3.205a1.2 1.2 0 110 2.4 1.2 1.2 0 010-2.4z"

        ligaIcono : String -> String -> SocialIcons -> Html msg
        ligaIcono direccion srCual iconoSocial =
            Html.a
                [ Attr.href direccion
                , class "text-gray-400 hover:text-gray-500"
                ]
                [ Html.span
                    [ class "sr-only" ]
                    [ Html.text srCual ]
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
    Html.footer
        []
        [ div
            []
            [ div
                [ class "bg-white py-4" ]
                []
            , div
                [ class "bg-gray-200" ]
                [ div
                    [ class "max-w-7xl mx-auto py-12 px-4 overflow-hidden sm:px-6 lg:px-8" ]
                    [ Html.nav
                        [ class "-mx-5 -my-2 flex flex-wrap justify-center"
                        , Aria.ariaLabel "Footer"
                        ]
                        [ ligaAlPie "#" "About"
                        , ligaAlPie "#" "Blog"
                        , ligaAlPie "#" "Jobs"
                        , ligaAlPie "#" "Press"
                        , ligaAlPie "#" "Accesibility"
                        , ligaAlPie "#" "Partners"
                        ]
                    , div
                        [ class "mt-8 flex justify-center space-x-6" ]
                        [ ligaIcono "facebook.com" "facebook" Facebook
                        , ligaIcono "instagram.com" "Instagram" Instagram
                        , ligaIcono "twitter.com" "Twitter" Twitter
                        , ligaIcono "github.com" "GitHub" Github
                        ]
                    , Html.p
                        [ class "mt-8 text-center text-base text-gray-500" ]
                        [ Html.text "&copy; 2020 Workflow, Inc. All rights reserved." ]
                    ]
                ]
            ]
        ]


type SocialIcons
    = Facebook
    | Instagram
    | Twitter
    | Github
