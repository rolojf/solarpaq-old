module Index exposing (view)

--import Pages.Metadata as Metadata exposing (Metadata)

import Data.Author
import Date
import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class)
import Html.Events
import Pages
import Pages.ImagePath as ImagePath exposing (ImagePath)
import Pages.PagePath as PagePath exposing (PagePath)
import TemplateMetadata exposing (BlogPost)
import TemplateType exposing (TemplateType)


view :
    List ( PagePath Pages.PathKey, TemplateType )
    -> Html msg
view posts =
    div [ class "columna"]
        (posts
            |> List.filterMap
                (\( path, metadata ) ->
                    case metadata of
                        TemplateType.BlogPost meta ->
                            if meta.draft then
                                Nothing

                            else
                                Just ( path, meta )

                        _ ->
                            Nothing
                )
            |> List.sortBy
                (\( path, metadata ) ->
                    -(metadata.published |> Date.toRataDie)
                )
            |> List.map postSummary
        )


postSummary :
    ( PagePath Pages.PathKey, BlogPost )
    -> Html msg
postSummary ( postPath, post ) =
    articleIndex post |> linkToPost postPath


linkToPost : PagePath Pages.PathKey -> Html msg -> Html msg
linkToPost postPath content =
    Html.a
        [ Attr.href <| PagePath.toString postPath ]
        [ content ]


title : String -> Html msg
title text =
    [ Html.text text ]
        |> Html.p
            [ class "text-3xl" ]


articleIndex : BlogPost -> Html msg
articleIndex metadata =
    div
        [ class "el"]
        [ postPreview metadata ]


postPreview : BlogPost -> Html msg
postPreview post =
    div
        [ class "columna"
        ]
        [ title post.title
        , div
            [ class "columna" ]
            [ Data.Author.view [ class "autor" ] post.author
            , Html.text post.author.name
            , Html.text "•"
            , Html.text (post.published |> Date.format "MMMM ddd, yyyy")
            ]
        , post.description
            |> Html.text
            |> List.singleton
            |> Html.p
                [ class "text-3xl text-center"]
        ]
