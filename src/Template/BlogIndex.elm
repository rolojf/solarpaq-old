module Template.BlogIndex exposing (Model, Msg, template)

import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class)
import Head
import Head.Seo as Seo
import Index
import Pages exposing (images)
import Pages.PagePath as PagePath exposing (PagePath)
import Pages.StaticHttp as StaticHttp
import Shared
import Site
import Template exposing (StaticPayload, TemplateWithState)
import TemplateMetadata exposing (BlogIndex)
import TemplateType exposing (TemplateType)


type Msg
    = Msg


template : TemplateWithState BlogIndex StaticData Model Msg
template =
    Template.withStaticData
        { head = head
        , staticData = staticData
        }
        |> Template.buildWithLocalState
            { view = view
            , init = init
            , update = update
            , subscriptions = \_ _ _ -> Sub.none
            }


staticData :
    List ( PagePath Pages.PathKey, TemplateType )
    -> StaticHttp.Request StaticData
staticData siteMetadata =
    StaticHttp.succeed
        [ "Del Showcase cambiado"
        , "Otra mas que definir"
        , "La Ultima finalmente"
        ]

    -- Showcase.staticRequest


type alias StaticData =
    List String --Showcase.Entry


init : BlogIndex -> ( Model, Cmd Msg )
init metadata =
    ( Model, Cmd.none )


update :
    BlogIndex
    -> Msg
    -> Model
    -> ( Model, Cmd Msg )
update metadata msg model =
    ( model, Cmd.none )


type alias Model =
    {}


view :
    Model
    -> List ( PagePath Pages.PathKey, TemplateType )
    -> StaticPayload BlogIndex StaticData
    -> Shared.RenderedBody
    -> Shared.PageView Msg
view model allMetadata staticPayload rendered =
    { title = "elm-pages blog"
    , body =
        [ div [ class "columna" ]
            [ div [ class "otra-columna" ]
                [ Index.view allMetadata
                ]
            ]
        ]
    }


head : StaticPayload BlogIndex StaticData -> List (Head.Tag Pages.PathKey)
head staticPayload =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = images.iconPng
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = Site.tagline
        , locale = Nothing
        , title = "elm-pages blog"
        }
        |> Seo.website
