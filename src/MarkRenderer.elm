module MarkRenderer exposing (view)

import Html exposing (Html)
import Html.Attributes as Attr exposing (class, property)
import Mark exposing (Block)
import Mark.Error


buildToc : TableOfContents
buildToc =
    [ { anchorId = "uno", name = "one", level = 1 }
    , { anchorId = "dos", name = "two", level = 2 }
    , { anchorId = "tres", name = "three", level = 3 }
    ]


type alias TableOfContents =
    List { anchorId : String, name : String, level : Int }


view : String -> Result String ( TableOfContents, List (Html msg) )
view textoCompleto =
    case Mark.compile documentar textoCompleto of
        Mark.Success loQueResulte ->
            Ok ( buildToc, [ loQueResulte ] )

        Mark.Almost partial ->
            Err <|
                String.join "\n" <|
                    List.map Mark.Error.toString partial.errors

        Mark.Failure errores ->
            Err <|
                String.join "\n" <|
                    List.map Mark.Error.toString errores



-- documentar


documentar : Mark.Document (Html msg)
documentar =
    Mark.document (Html.div []) <|
        Mark.manyOf
            [ header1
            , header2
            , header3
            , header4
            , image
            , list
            , bloqueCode
            , div1
            , Mark.map (Html.p []) text
            ]


procesaTexto : Mark.Styles -> String -> Html msg
procesaTexto estilos texto =
    case ( estilos.bold, estilos.italic, estilos.strike ) of
        ( True, False, False ) ->
            Html.strong [ class "font-bold" ] [ Html.text texto ]

        ( True, True, False ) ->
            Html.strong [ class "font-bold" ] [ Html.em [ class "italic" ] [ Html.text texto ] ]

        ( True, False, True ) ->
            Html.strong [ class "font-bold" ] [ Html.s [ class "line-throught" ] [ Html.text texto ] ]

        ( True, True, True ) ->
            Html.strong
                [ class "font-bold" ]
                [ Html.em [ class "italic" ]
                    [ Html.s [ class "line-throught" ]
                        [ Html.text texto ]
                    ]
                ]

        ( False, True, False ) ->
            Html.em [ class "italic" ] [ Html.text texto ]

        ( False, True, True ) ->
            Html.em [ class "italic" ] [ Html.s [ class "line-throught" ] [ Html.text texto ] ]

        ( False, False, True ) ->
            Html.s [ class "line-through" ] [ Html.text texto ]

        ( False, False, False ) ->
            Html.span [ class "non-italic" ] [ Html.text texto ]


textoSimple : Block (List (Html msg))



--textoSimple : (Mark.Styles -> String -> Html msg) -> Block (List (Html msg))


textoSimple =
    Mark.text procesaTexto


text =
    Mark.textWith
        { view =
            \styles string ->
                procesaTexto styles string
        , replacements = Mark.commonReplacements
        , inlines =
            [ especialLink
            , talCualDrop
            ]
        }



-- Inlines


especialLink =
    Mark.annotation "link"
        (\texts url ->
            Html.a [ Attr.href url ] <|
                List.map (\( styles, str ) -> procesaTexto styles str) texts
        )
        |> Mark.field "url" Mark.string


talCualDrop =
    Mark.verbatim "drop"
        (\str ->
            let
                drop =
                    String.left 1 str

                lede =
                    String.dropLeft 1 str
            in
            Html.span []
                [ Html.span [ Attr.class "drop-capital" ]
                    [ Html.text drop ]
                , Html.span [ Attr.class "lede" ]
                    [ Html.text lede ]
                ]
        )


talCualName =
    Mark.verbatim "name"
        (\str -> Html.code [] [ Html.text str ])



-- bloques


bloqueCode =
    Mark.record "Code"
        (\lang str ->
            Html.pre [ class lang ] [ Html.text str ]
        )
        |> Mark.field "lang" Mark.string
        |> Mark.field "code" Mark.string
        |> Mark.toBlock



-- Mark.block : String -> (child -> result) -> Block child -> Block result
--              "H1" -> (List (Html msg) -> Html msg) -> Block (List (Html msg)) -> Block (Html msg)


header1 : Block (Html msg)
header1 =
    Mark.block "H1"
        (Html.h1 [])
        (Mark.text procesaTexto)

-- Mark.text (Styles -> String -> result) -> Block (List result)


header2 =
    Mark.block "H2" (Html.h1 []) (Mark.text procesaTexto)


header3 =
    Mark.block "H3" (Html.h1 []) (Mark.text procesaTexto)


header4 =
    Mark.block "H4" (Html.h1 []) (Mark.text procesaTexto)


div1 : Block (Html msg)
div1 =
    Mark.record "Div"
        (\clases contenido ->
            Html.div
                [ class clases ]
                [ Html.text contenido ]
        )
        |> Mark.field "class" Mark.string
        -- field: String -> Block value -> Record (value -> result) -> Record result
        |> Mark.field "content" Mark.string
        -- Mark.text (Styles -> String -> result) -> Block (List result)
        |> Mark.toBlock
            -- Mark.block : String -> (child -> result) -> Block child -> Block result
            --              "H1" -> (List (Html msg) -> Html msg) -> Block (List (Html msg)) -> Block (Html msg)
{-            Mark.block
            "H1"
            (Html.h1 [])
            (Mark.text procesaTexto)
-}


--            (Mark.text procesaTexto)
{-
div2 : Mark.Block (Html msg)

list =
    Mark.tree "Div2" renderListDiv (Mark.map (Html.div []) text)


renderListDiv : Mark.Enumerated (Html msg) -> Html msg
renderListDiv (Mark.Enumerated enum) =
    Html.div []
        (List.map renderItemDiv enum.items)


renderItem : Mark.Item (Html msg) -> Html msg
renderItem (Mark.Item item) =
    Html.p []
        item.content
        :: renderListDiv item.children
-}

image =
    Mark.record "Image"
        (\src description ->
            Html.img
                [ Attr.src src
                , Attr.alt description
                , Attr.style "float" "left"
                , Attr.style "margin-right" "48px"
                ]
                []
        )
        |> Mark.field "src" Mark.string
        |> Mark.field "description" Mark.string
        |> Mark.toBlock


list : Mark.Block (Html msg)
list =
    Mark.tree "List" renderList (Mark.map (Html.div []) text)


renderList : Mark.Enumerated (Html msg) -> Html msg
renderList (Mark.Enumerated enum) =
    let
        group =
            case enum.icon of
                Mark.Bullet ->
                    Html.ul

                Mark.Number ->
                    Html.ol
    in
    group []
        (List.map renderItem enum.items)


renderItem : Mark.Item (Html msg) -> Html msg
renderItem (Mark.Item item) =
    Html.li []
        [ Html.div [] item.content
        , renderList item.children
        ]



-- helpers


erroresToHtml errores =
    Html.pre [] [ Html.text <| String.join "\n" <| List.map Mark.Error.toString errores ]


rawTextToId : String -> String
rawTextToId rawText =
    rawText
        |> String.split " "
        |> String.join "-"
        |> String.toLower
