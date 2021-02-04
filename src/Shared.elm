module Shared exposing (Model, Msg(..), PageView, RenderedBody, SharedMsg(..), StaticData, template)

import DocumentSvg
import FontAwesome
import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class)
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
    , title = pageView.title
    }


incrementView : Model -> Html Msg
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
