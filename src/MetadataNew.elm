module MetadataNew exposing (decoder)

import Json.Decode as Decode exposing (Decoder)
import Template.BlogPost
import Template.Page
import TemplateType exposing (TemplateType)


decoder : Decoder TemplateType
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\pageType ->
                case pageType of
                    "page" ->
                        Template.Page.decoder
                            |> Decode.map TemplateType.Page

                    "blog-index" ->
                        Decode.succeed {}
                            |> Decode.map TemplateType.BlogIndex

                    "blog" ->
                        Template.BlogPost.decoder
                            |> Decode.map TemplateType.BlogPost

                    "selComp1" ->
                        Decode.field "title" Decode.string
                            |> Decode.map (\title -> TemplateType.SelComp1 { title = title })

                    _ ->
                        Decode.fail <| "Pagina con \"tipo\" inesperado " ++ pageType
            )
