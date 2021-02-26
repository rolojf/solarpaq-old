module MetadataNew exposing (decoder)

import Json.Decode as Decode exposing (Decoder)
import Template.BlogIndex
import Template.BlogPost
import Template.Home
import Template.Page
import Template.SelComp1
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
                        Template.BlogIndex.decoder
                            |> Decode.map TemplateType.BlogIndex

                    "blog" ->
                        Template.BlogPost.decoder
                            |> Decode.map TemplateType.BlogPost

                    "selComp1" ->
                        Template.SelComp1.decoder
                            |> Decode.map TemplateType.SelComp1

                    "home" ->
                        Template.Home.decoder
                            |> Decode.map TemplateType.Home

                    _ ->
                        Decode.fail <| "Pagina con \"tipo\" inesperado " ++ pageType
            )
