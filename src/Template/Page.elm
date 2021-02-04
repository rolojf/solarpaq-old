module Template.Page exposing (Model, Msg, decoder, template)

import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class)
import Head
import Head.Seo as Seo
import Json.Decode as Decode
import Pages exposing (images)
import Pages.PagePath exposing (PagePath)
import Shared
import Site
import Template exposing (StaticPayload, Template, TemplateWithState)
import TemplateMetadata exposing (Page)
import TemplateType exposing (TemplateType)


type alias Model =
    ()


type alias Msg =
    Never


template : Template Page ()
template =
    Template.noStaticData { head = head }
        |> Template.buildNoState { view = view }


decoder : Decode.Decoder Page
decoder =
    Decode.map Page
        (Decode.field "title" Decode.string)


head :
    StaticPayload Page ()
    -> List (Head.Tag Pages.PathKey)
head { metadata } =
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
        , title = metadata.title
        }
        |> Seo.website


view :
    List ( PagePath Pages.PathKey, TemplateType )
    -> StaticPayload Page ()
    -> Shared.RenderedBody
    -> Shared.PageView msg
view allMetadata { metadata } rendered =
    { title = metadata.title
    , body =
        [ [ div [ class "columna" ]
                (Tuple.second rendered |> List.map (Html.map never))
          ]
            |> div [ class "text-column"]
        ]
    }
