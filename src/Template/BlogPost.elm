module Template.BlogPost exposing (Model, Msg, decoder, template)

import Data.Author as Author exposing (Author)
import Date exposing (Date)
import Head
import Head.Seo as Seo
import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class)
import Json.Decode as Decode
import List.Extra
import Pages
import Pages.ImagePath as ImagePath exposing (ImagePath)
import Pages.PagePath as PagePath exposing (PagePath)
import Palette
import Shared
import Site
import StructuredData
import Template exposing (StaticPayload, Template, TemplateWithState)
import TemplateMetadata exposing (BlogPost)
import TemplateType exposing (TemplateType)


type alias Model =
    ()


type alias Msg =
    Never


template : Template BlogPost ()
template =
    Template.noStaticData { head = head }
        |> Template.buildNoState { view = view }


decoder : Decode.Decoder BlogPost
decoder =
    Decode.map6 BlogPost
        (Decode.field "title" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "published"
            (Decode.string
                |> Decode.andThen
                    (\isoString ->
                        case Date.fromIsoString isoString of
                            Ok date ->
                                Decode.succeed date

                            Err error ->
                                Decode.fail error
                    )
            )
        )
        (Decode.field "author" Author.decoder)
        -- (Decode.field "image" imageDecoder)
        (Decode.field "draft" Decode.bool
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault False)
        )
        (Decode.succeed -- definir aquí como va el menú
            [ { direccion = "uno"
              , queDice = "One"
              }
            , { direccion = "dos"
              , queDice = "Two"
              }
            ]
        )


imageDecoder : Decode.Decoder (ImagePath Pages.PathKey)
imageDecoder =
    Decode.string
        |> Decode.andThen
            (\imageAssetPath ->
                case findMatchingImage imageAssetPath of
                    Nothing ->
                        Decode.fail "Couldn't find image."

                    Just imagePath ->
                        Decode.succeed imagePath
            )


findMatchingImage : String -> Maybe (ImagePath Pages.PathKey)
findMatchingImage imageAssetPath =
    List.Extra.find (\image -> ImagePath.toString image == imageAssetPath) Pages.allImages


view :
    List ( PagePath Pages.PathKey, TemplateType )
    -> StaticPayload BlogPost ()
    -> Shared.RenderedBody
    -> Shared.PageView msg
view allMetadata { metadata } rendered =
    { title = metadata.title
    , body =
        [ div [ class "columna-en-blog-post-temp" ]
            [ div [ class "otra-columna-en-blog-post-temp" ]
                (div [ class "otra-2-columna-en-blog-post-temp" ]
                    [ div [ class "renglon-en-blog-post-temp" ]
                        [ Author.view [] metadata.author
                        , div [ class "otra-3-columna-en-blog-post-temp" ]
                            [ Html.p [ class "font-bold set-font-en-blog-post-temp" ]
                                [ Html.text metadata.author.name ]
                            , Html.p [ class "text-2xl set-text-size-en-blog-post-temp" ]
                                [ Html.text metadata.author.bio ]
                            ]
                        ]
                    ]
                    :: div [ class "el-en-blog-post-temp text-2xl text-gray-600" ]
                        [ publishedDateView metadata ]
                    :: Palette.blogHeading metadata.title
                    :: Tuple.second rendered
                    |> List.map (Html.map never)
                )
            ]
        ]
    }



-- :: articleImageView metadata.image


head :
    StaticPayload BlogPost ()
    -> List (Head.Tag Pages.PathKey)
head { metadata, path } =
    Head.structuredData
        (StructuredData.article
            { title = metadata.title
            , description = metadata.description
            , author = StructuredData.person { name = metadata.author.name }
            , publisher = StructuredData.person { name = "Dillon Kearns" }
            , url = Site.canonicalUrl ++ "/" ++ PagePath.toString path
            , imageUrl = "" -- Site.canonicalUrl ++ "/" ++ ImagePath.toString metadata.image
            , datePublished = Date.toIsoString metadata.published
            , mainEntityOfPage =
                StructuredData.softwareSourceCode
                    { codeRepositoryUrl = "https://github.com/dillonkearns/elm-pages"
                    , description = "A statically typed site generator for Elm."
                    , author = "Dillon Kearns"
                    , programmingLanguage = StructuredData.elmLang
                    }
            }
        )
        :: (Seo.summaryLarge
                { canonicalUrlOverride = Nothing
                , siteName = "elm-pages"
                , image =
                    { url = Pages.images.articleCovers.hello
                    , alt = metadata.description
                    , dimensions = Nothing
                    , mimeType = Nothing
                    }
                , description = metadata.description
                , locale = Nothing
                , title = metadata.title
                }
                |> Seo.article
                    { tags = []
                    , section = Nothing
                    , publishedTime = Just (Date.toIsoString metadata.published)
                    , modifiedTime = Nothing
                    , expirationTime = Nothing
                    }
           )


publishedDateView : { a | published : Date.Date } -> Html msg
publishedDateView metadata =
    Html.text
        (metadata.published
            |> Date.format "MMMM ddd, yyyy"
        )


articleImageView : ImagePath Pages.PathKey -> Html msg
articleImageView articleImage =
    Html.img
        [ class "imagen"
        , Attr.src <| ImagePath.toString articleImage
        , Attr.alt "Article cover photo"
        ]
        []
