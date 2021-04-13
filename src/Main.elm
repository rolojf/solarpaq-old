module Main exposing (main)

import Color
import Data.Author
import Head
import MarkdownRenderer
import MarkRenderer
import MetadataNew
import MimeType
import Pages exposing (images, pages)
import Pages.ImagePath exposing (ImagePath)
import Pages.Manifest as Manifest
import Pages.Manifest.Category
import Pages.PagePath as PagePath exposing (PagePath)
import Pages.Platform
import Shared
import Site
import TemplateModulesBeta
import TemplateType exposing (TemplateType)


--main : Pages.Platform.Program Model Msg Metadata View Pages.PathKey

main : Pages.Platform.Program TemplateModulesBeta.Model TemplateModulesBeta.Msg TemplateType Shared.RenderedBody Pages.PathKey
main =
    TemplateModulesBeta.mainTemplate
        { documents =
            [ { extension = "md"
              , metadata = MetadataNew.decoder -- metadata parser/decoder?
              , body = MarkdownRenderer.view -- body parser?
              }
            ]
        , site = Site.config
        }
        |> Pages.Platform.toProgram
