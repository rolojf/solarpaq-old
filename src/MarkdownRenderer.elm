module MarkdownRenderer exposing (TableOfContents, view)

import Dotted
import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class)
import Json.Encode as Encode exposing (Value)
import Markdown.Block as Block exposing (Block, Inline, ListItem(..), Task(..))
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import Oembed
import Palette
import SyntaxHighlight


buildToc : List Block.Block -> TableOfContents
buildToc blocks =
    let
        headings =
            gatherHeadings blocks
    in
    headings
        |> List.map Tuple.second
        |> List.map
            (\styledList ->
                { anchorId = styledToString styledList |> rawTextToId
                , name = styledToString styledList
                , level = 1
                }
            )


type alias TableOfContents =
    List { anchorId : String, name : String, level : Int }


view : String -> Result String ( TableOfContents, List (Html msg) )
view markdown =
    case
        markdown
            |> Markdown.Parser.parse
    of
        Ok okAst ->
            case Markdown.Renderer.render myRenderer okAst of
                Ok rendered ->
                    Ok ( buildToc okAst, rendered )

                Err errors ->
                    Err errors

        Err error ->
            Err (error |> List.map Markdown.Parser.deadEndToString |> String.join "\n")


myRenderer : Markdown.Renderer.Renderer (Html msg)
myRenderer =
    let
        defaultOne =
            Markdown.Renderer.defaultHtmlRenderer
    in
    { defaultOne | html = htmls }


htmls =
    --Markdown.Html.oneOf
    --    [
    Markdown.Html.tag "div"
        (\clase quienSoy children ->
            div
                [ case clase of
                    Just claseDef ->
                        class claseDef

                    Nothing ->
                        class "ninguna"
                , case quienSoy of
                    Just soyYoMero ->
                        Attr.id soyYoMero

                    Nothing ->
                        Attr.id "indefinido"
                ]
                children
        )
        |> Markdown.Html.withOptionalAttribute "class"
        |> Markdown.Html.withOptionalAttribute "id"



--    ]


styledToString : List Inline -> String
styledToString inlines =
    --List.map .string list
    --|> String.join "-"
    -- TODO do I need to hyphenate?
    inlines
        |> Block.extractInlineText


gatherHeadings : List Block -> List ( Block.HeadingLevel, List Inline )
gatherHeadings blocks =
    List.filterMap
        (\block ->
            case block of
                Block.Heading level content ->
                    Just ( level, content )

                _ ->
                    Nothing
        )
        blocks


rawTextToId : String -> String
rawTextToId rawText =
    rawText
        |> String.split " "
        |> String.join "-"
        |> String.toLower


passThroughNode nodeName =
    Markdown.Html.tag nodeName
        (\id class href children ->
            Html.node nodeName
                ([ id |> Maybe.map Attr.id
                 , class |> Maybe.map Attr.class
                 , href |> Maybe.map Attr.href
                 ]
                    |> List.filterMap identity
                )
                children
        )
        |> Markdown.Html.withOptionalAttribute "id"
        |> Markdown.Html.withOptionalAttribute "class"
        |> Markdown.Html.withOptionalAttribute "href"
