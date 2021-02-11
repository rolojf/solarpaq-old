module Shared exposing (Model, Msg(..), PageView, RenderedBody, SharedMsg(..), StaticData, template)

import DocumentSvg
import FontAwesome
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
    | Increment
    | SharedMsg SharedMsg


type alias StaticData =
    Int


type SharedMsg
    = IncrementFromChild


type alias Model =
    { showMobileMenu : Bool
    , counter : Int
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
            [ myNav model |> Html.map toMsg
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
                    []
                ]
            ]
    , title = pageView.title
    }



{- oldViewBody =
   if model.showMobileMenu then
       div [ class "columna-en-el-shared" ]
           [ div
               [ class "renglon-en-blog-post-temp" ]
               [ logoLinkMobile
                   |> Html.map toMsg
               , FontAwesome.styledIcon "fas fa-bars"
                   [ Html.Events.onClick ToggleMobileMenu
                   , class "w-7 h-7"
                   ]
                   |> Html.map toMsg
               ]
           , div
               [ class "flex items-center justify-between" ]
               (navbarLinks stars page.path)
           ]

   else
       div [ class "columna-en-blog-post-temp" ]
           (List.concat
               [ [ header stars page.path |> Html.map toMsg ]
               , [ incrementView model |> Html.map toMsg ]
               , pageView.body
               ]
           )
-}


incrementView model =
    div [ Html.Events.onClick Increment ]
        [ Html.text <| "Shared count: " ++ String.fromInt model.counter ]


logoLinkMobile =
    Html.a
        [ Attr.href "/"
        , class "row navbar-title"
        ]
        [ Html.text "elm-pages" ]


navbarLinks stars currentPath =
    [ elmDocsLink
    , githubRepoLink stars
    , highlightableLink currentPath pages.blog.directory "Blog"
    ]


header : Int -> PagePath Pages.PathKey -> Html Msg
header stars currentPath =
    div [ class "columna mb-24" ]
        [ responsiveHeader
        , div [ class "otra-columna" ]
            [ div [ class "row" ]
                [ logoLink
                , div [ class "w-48 ml-20 flex items-center justify-between" ]
                    (navbarLinks stars currentPath)
                ]
            ]
        ]


logoLink =
    Html.a
        [ Attr.href "/"
        , class "row navbar-title"
        ]
        [ div [ class "h-8 w-8" ]
            [ DocumentSvg.view
            , Html.text "elm-pages"
            ]
        ]


responsiveHeader =
    div [ class "column" ]
        [ logoLinkMobile
        , div
            [ class "el"
            , Html.Events.onClick ToggleMobileMenu
            ]
            [ FontAwesome.styledIcon "fas fa-bars" [ class "w-6 h-6" ]
            ]
        ]


githubRepoLink : Int -> Html msg
githubRepoLink starCount =
    Html.a
        [ Attr.href "https://github.com/dillonkearns/elm-pages" ]
        [ Html.img
            [ Attr.src <| ImagePath.toString Pages.images.github
            , Attr.alt "Github repo"
            , class "w-8 h-8"
            ]
            []
        , Html.text <| String.fromInt starCount
        ]


elmDocsLink : Html msg
elmDocsLink =
    Html.a
        [ Attr.href "https://package.elm-lang.org/packages/dillonkearns/elm-pages/latest/"
        ]
        [ Html.img
            [ Attr.src <| ImagePath.toString Pages.images.elmLogo
            , Attr.alt "Elm Package Docs"
            , class "w-8 h-8"
            ]
            []
        ]


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


myNav : Model -> Html Msg
myNav modelo =
    Html.nav
        [ class "bg-gray-800" ]
        [ div
            [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
            [ div
                [ class "flex items-center justify-between h-16" ]
                [ myLogoAndLinks
                , myHiddenBlock
                ]
            , mobileMenuButton modelo
            ]
        , myHiddenMenu
        ]



{-
   Mobile menu, toggle classes based on menu state.
   Open: "block", closed: "hidden"
-}


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
                -- Current: "bg-gray-900 text-white",
                -- Default: "text-gray-300 hover:bg-gray-700 hover:text-white"
                (Html.a
                    [ Attr.href "#"
                    , class "bg-gray-900 text-white px-3 py-2 rounded-md text-sm font-medium"
                    ]
                    [ Html.text "Dashboard" ]
                    :: ligasChulas " text-sm"
                )
            ]
        ]


myHiddenBlock : Html Msg
myHiddenBlock =
    div
        [ class "hidden md:block" ]
        [ div
            [ class "ml-4 flex items-center md:ml-6" ]
            [ Html.button
                [ class "bg-gray-800 p-1 rounded-full text-gray-400 hover:text-white focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-gray-800 focus:ring-white"

                -- , Html.Events.onClick ToggleMobileMenu
                ]
                [ Html.span
                    [ class "sr-only" ]
                    [ Html.text "View notifications" ]

                -- Heroicon name: outline/bell
                , heroiconOutlineBell
                ]

            -- Profile dropdown --
            , div
                [ class "ml-3 relative" ]
                [ div
                    []
                    [ Html.button
                        [ class "max-w-xs bg-gray-800 rounded-full flex items-center text-sm text-white focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-gray-800 focus:ring-white"
                        , Attr.id "user-menu"
                        , Aria.ariaHasPopup "true"
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

                -- Profile dropdown panel, show/hide based on dropdown state.
                {- Entering: "transition ease-out duration-100"
                     From: "transform opacity-0 scale-95"
                     To: "transform opacity-100 scale-100"
                   Leaving: "transition ease-in duration-75"
                     From: "transform opacity-100 scale-100"
                     To: "transform opacity-0 scale-95"
                -}
                , div
                    [ class "origin-top-right absolute right-0 mt-2 w-48 rounded-md shadow-lg py-1 bg-white ring-1 ring-black ring-opacity-5"
                    , Aria.role "menu"

                    -- , Aria.aria-orientation "vertical"
                    , Aria.ariaLabelledby "user-menu"
                    ]
                    (menuItems "block px-4 py-2 text-sm text-gray-700 hover:bg-gray-100")
                ]
            ]
        ]


myHiddenMenu : Html msg
myHiddenMenu =
    div
        [ class "hidden md:hidden" ]
        [ div
            [ class "px-2 pt-2 pb-3 space-y-1 sm:px-3" ]
            -- Current: "bg-gray-900 text-white", Default: "text-gray-300 hover:bg-gray-700 hover:text-white" --
            (Html.a
                [ Attr.href "#"
                , class "bg-gray-900 text-white block px-3 py-2 rounded-md text-base font-medium"
                ]
                [ Html.text "Dashboard" ]
                :: ligasChulas " block text-base"
            )
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
                , Html.button
                    [ class "ml-auto bg-gray-800 flex-shrink-0 p-1 rounded-full text-gray-400 hover:text-white focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-gray-800 focus:ring-white" ]
                    [ Html.span
                        [ class "sr-only" ]
                        [ Html.text "View notifications" ]
                    , heroiconOutlineBell
                    ]
                ]
            , div [ class "mt-3 px-2 space-y-1" ]
                (menuItems "block px-3 py-2 rounded-md text-base font-medium text-gray-400 hover:text-white hover:bg-gray-700")
            ]
        ]


ligasChulas : String -> List (Html msg)
ligasChulas extraClases =
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



-- Heroicon name: outline/menu
-- Menu open: "hidden", Menu closed: "block"


heroiconOutlineMenu : Model -> Html msg
heroiconOutlineMenu modelo =
    svg
        [ Svg.Attributes.class <|
            "h-6 w-6 "
                ++ (if modelo.showMobileMenu then
                        "hidden"

                    else
                        "block"
                   )

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



-- Heroicon name: outline/x
--  Menu open: "block", Menu closed: "hidden"


heroiconOutlineX : Model -> Html msg
heroiconOutlineX modelo =
    svg
        [ Svg.Attributes.class <|
            "h-6 w-6 "
                ++ (if modelo.showMobileMenu then
                        "block"

                    else
                        "hidden"
                   )

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



-- Heroicon name: outline/bell --


heroiconOutlineBell : Html msg
heroiconOutlineBell =
    svg
        [ Svg.Attributes.class "h-6 w-6"

        --, mlns="http://www.w3.org/2000/svg"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.viewBox "0 0 24 24"
        , Svg.Attributes.stroke "currentColor"

        --, aria-hidden="true"
        ]
        [ path
            [ Svg.Attributes.strokeLinecap "round"
            , Svg.Attributes.strokeLinejoin "round"
            , Svg.Attributes.strokeWidth "2"
            , Svg.Attributes.d "M15 17h5l-1.405-1.405A2.032 2.032 0 0118 14.158V11a6.002 6.002 0 00-4-5.659V5a2 2 0 10-4 0v.341C7.67 6.165 6 8.388 6 11v3.159c0 .538-.214 1.055-.595 1.436L4 17h5m6 0v1a3 3 0 11-6 0v-1m6 0H9"
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
            , heroiconOutlineMenu modelo
            , heroiconOutlineX modelo
            ]
        ]
