module MetadataNew exposing (decoder)

import Json.Decode as Decode exposing (Decoder)
import Template.BlogPost
import Template.Page
import TemplateType exposing (TemplateType)
import Template.BlogPost

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

                    _ ->
                        Decode.fail <| "Unexpected page \"type\" " ++ pageType
            )

