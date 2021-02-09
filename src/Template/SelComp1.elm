module Template.SelComp1 exposing (Model, Msg, decoder, template)

import Head
import Head.Seo as Seo
import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class)
import Json.Decode as Decode
import MarkdownRenderer
import Pages exposing (images)
import Pages.PagePath as PagePath exposing (PagePath)
import Pages.StaticHttp as StaticHttp
import Palette
import Shared
import Site
import Template exposing (StaticPayload, TemplateWithState)
import TemplateMetadata exposing (SelComp1)
import TemplateType exposing (TemplateType)


type alias StaticData =
    ()


type alias Model =
    {}


type Msg
    = Increment


template : TemplateWithState SelComp1 StaticData Model Msg
template =
    Template.noStaticData { head = head }
        |> Template.buildWithSharedState
            { view = view
            , init = init
            , update = update
            , subscriptions = \_ _ _ _ -> Sub.none
            }


init : SelComp1 -> ( Model, Cmd Msg )
init metadata =
    ( {}, Cmd.none )


update : SelComp1 -> Msg -> Model -> Shared.Model -> ( Model, Cmd Msg, Maybe Shared.SharedMsg )
update metadata msg model sharedModel =
    case msg of
        Increment ->
            ( model, Cmd.none, Just Shared.IncrementFromChild )


staticData :
    List ( PagePath Pages.PathKey, TemplateType )
    -> StaticHttp.Request StaticData
staticData siteMetadata =
    StaticHttp.succeed ()


decoder : Decode.Decoder SelComp1
decoder =
    Decode.map SelComp1
        (Decode.field "title" Decode.string)


head : StaticPayload SelComp1 StaticData -> List (Head.Tag Pages.PathKey)
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
        , title = staticPayload.metadata.title
        }
        |> Seo.website


view :
    Model
    -> Shared.Model
    -> List ( PagePath Pages.PathKey, TemplateType )
    -> StaticPayload SelComp1 StaticData
    -> Shared.RenderedBody
    -> Shared.PageView Msg
view model sharedModel allMetadata staticPayload rendered =
    { title = staticPayload.metadata.title
    , body =
        [ div [ class "columna cero" ]
            [ counterView sharedModel
            , Html.text "Aquí estaba el listado de la documentación"
            , div [ class "primera columna" ]
                [ Palette.heading 1 [ Html.text staticPayload.metadata.title ]
                , div [ class "segunda columna" ]
                    [ tocView staticPayload.path (Tuple.first rendered)
                    , div
                        [ class "tercer columna" ]
                        (Tuple.second rendered |> List.map (Html.map never))
                    ]
                ]
            ]
        ]
    }


counterView : Shared.Model -> Html Msg
counterView sharedModel =
    Html.text <| "Docs count: " ++ String.fromInt sharedModel.counter


tocView : PagePath Pages.PathKey -> MarkdownRenderer.TableOfContents -> Html msg
tocView path toc =
    div [ class "toc uno" ]
        [ Html.text "Table of Contents"
        , div [ class "toc dos" ]
            (toc
                |> List.map
                    (\heading ->
                        Html.a
                            [ Attr.href
                                (PagePath.toString path ++ "#" ++ heading.anchorId)
                            , class "clase de ligas"
                            ]
                            [ Html.text heading.name ]
                    )
            )
        ]
