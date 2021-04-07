module Template.Home exposing (Model, Msg, decoder, template)

import Browser.Dom as Dom
import Css
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Attributes.Aria as Aria
import Html.Styled as Htmls exposing (div)
import Html.Styled.Attributes as Attr exposing (class)
import Html.Styled.Events as Events
import Json.Decode as Decode
import MarkdownRenderer
import Pages exposing (images)
import Pages.PagePath as PagePath exposing (PagePath)
import Pages.StaticHttp as StaticHttp
import Shared exposing (UsuarioStatus)
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
    -> StaticPayload Home StaticData
    -> Shared.RenderedBody
    -> Shared.PageView Msg
view model sharedModel allMetadata staticPayload rendered =
    { title = staticPayload.metadata.title
    , body =
        [ div
            [ Attr.css [ Tw.max_w_7xl, Tw.mx_auto, TwBp.lg [ Tw.px_8], TwBp.sm [ Tw.px_6] ] ]
            [ counterView sharedModel
            , Htmls.br [] []
            , Htmls.text "Aquí estaba el listado de la documentación"
            , div [ class "segunda columna" ]
                [ tocView staticPayload.path (Tuple.first rendered)
                , div
                    [ Attr.css [ Tw.prose, TwBp.lg [ Tw.prose_xl ] ] ]
                    (Tuple.second rendered
                        |> List.map (Html.map never)
                        |> List.map Htmls.fromUnstyled
                    )
                ]
            ]
            |> Htmls.toUnstyled
        ]
    }


counterView : Shared.Model -> Htmls.Html Msg
counterView sharedModel =
    Htmls.text <| "Docs count: " ++ String.fromInt sharedModel.counter


tocView : PagePath Pages.PathKey -> MarkdownRenderer.TableOfContents -> Htmls.Html msg
tocView path toc =
    div [ class "toc uno" ]
        [ Htmls.text "Table of Contents"
        , div [ class "toc dos" ]
            (toc
                |> List.map
                    (\heading ->
                        Htmls.a
                            [ Attr.href
                                (PagePath.toString path ++ "#" ++ heading.anchorId)
                            , class "clase de ligas"
                            ]
                            [ Htmls.text heading.name ]
                    )
            )
        ]
