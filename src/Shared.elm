module Shared exposing (Model, Msg(..), PageView, RenderedBody, SharedMsg(..), StaticData, template)

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
import Svg exposing (path, svg)
import Svg.Attributes
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


type SharedMsg
    = IncrementFromChild


type alias Model =
    { showMobileMenu : Bool
    , counter : Int
    , showProfileMenu : Bool
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
                    [ class "max-w-7xl mx-auto py-6 sm:px-6 lg:px-8" ]
                    ((incrementView model
                        |> Html.map toMsg
                     )
                        :: pageView.body
                    )
                ]
            , Html.footer
                [ class "px-4 py-4 sm:px-0" ]
                [ div
                    [ class "border-4 border-dashed border-gray-200 rounded-lg h-96" ]
                    [ Html.text <| Debug.toString <| PagePath.toString page.path
                    ]
                ]
            ]
    , title = pageView.title
    }


incrementView model =
    div [ Html.Events.onClick Increment ]
        [ Html.text <| "Shared count: " ++ String.fromInt model.counter ]



-- navbarLinks stars currentPath =
--                    (navbarLinks stars currentPath)


highlightableLink :
    PagePath Pages.PathKey
    -> Directory Pages.PathKey Directory.WithIndex
    -> String
    -> Html msg
highlightableLink currentPath linkDirectory displayName =
    let
        isHighlighted =
            currentPath |> Directory.includes linkDirectory
    in
    Html.a
        [ Attr.href
            (linkDirectory
                |> Directory.indexPath
                |> PagePath.toString
            )
        , class <|
            if isHighlighted then
                "highlited"

            else
                "nada"
        ]
        [ Html.text displayName ]


myNav :
    Model
    ->
        { path : PagePath Pages.PathKey
        , frontmatter : TemplateType
        }
    -> Html Msg
myNav modelo page =
    Html.nav
        [ class "bg-gray-800" ]
        [ div
            [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
            [ div
                [ class "flex items-center justify-between h-16" ]
                [ myLogoAndLinks page
                , myHiddenBlock modelo
                , mobileMenuButton modelo
                ]
            ]
        , myHiddenMenu page modelo
        ]


getMenu :
    { path : PagePath Pages.PathKey
    , frontmatter : TemplateType
    }
    -> List TemplateMetadata.Liga
getMenu page =
    case page.frontmatter of
        TemplateType.Page { menu } ->
            menu

        TemplateType.BlogPost { menu } ->
            menu

        TemplateType.BlogIndex { menu } ->
            menu

        TemplateType.SelComp1 { menu } ->
            menu


myLogoAndLinks :
    { path : PagePath Pages.PathKey
    , frontmatter : TemplateType
    }
    -> Html msg
myLogoAndLinks page =
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
                (ligasChulas False page <| getMenu page)
            ]
        ]


myHiddenBlock : Model -> Html Msg
myHiddenBlock modelo =
    div
        [ class "hidden md:block" ]
        [ div
            [ class "ml-4 flex items-center md:ml-6" ]
            [ div             -- Profile dropdown --
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


myHiddenMenu :
    { path : PagePath Pages.PathKey
    , frontmatter : TemplateType
    }
    -> Model
    -> Html msg
myHiddenMenu page modelo =
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
            (ligasChulas True page <| getMenu page)
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
    ->
        { path : PagePath Pages.PathKey
        , frontmatter : TemplateType
        }
    -> List TemplateMetadata.Liga
    -> List (Html msg)
ligasChulas esMovil page menus =
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
            ->
                { path : PagePath Pages.PathKey
                , frontmatter : TemplateType
                }
            -> TemplateMetadata.Liga
            -> String
        clasesQueVan esParaMovil pagina liga =
            if PagePath.toString pagina.path == liga.direccion then
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
                (clasesQueVan esMovil page algoDelMenu)
                algoDelMenu
        )
        menus



{-
       [ Html.a
       [ Attr.href "#"
       , class <| "text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md font-medium" ++ extraClases
       ]
       [ Html.text "Team" ]
   , Html.a
       [ Attr.href "#"
       , class <| "text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md font-medium" ++ extraClases
       ]
       [ Html.text "Projects" ]
   , Html.a
       [ Attr.href "#"
       , class <| "text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md font-medium" ++ extraClases
       ]
       [ Html.text "Calendar" ]
   , Html.a
       [ Attr.href "#"
       , class <| "text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md font-medium" ++ extraClases
       ]
       [ Html.text "Reports" ]
   ]
-}


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


mobileMenuButton : Model -> Html Msg
mobileMenuButton modelo =
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
