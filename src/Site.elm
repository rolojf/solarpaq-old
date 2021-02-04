module Site exposing (canonicalUrl, config, tagline)

import Color
import MimeType
import Pages exposing (images, pages)
import Pages.ImagePath exposing (ImagePath)
import Pages.Manifest as Manifest
import Pages.Manifest.Category
import Pages.PagePath exposing (PagePath)
import Pages.StaticHttp as StaticHttp
import TemplateType exposing (TemplateType)


type alias SiteConfig =
    { canonicalUrl : String
    , manifest : Manifest.Config Pages.PathKey
    }


config : SiteConfig
config =
    { canonicalUrl = canonicalUrl
    , manifest = manifest
    }


type alias StaticData =
    ()


staticData :
    List ( PagePath Pages.PathKey, TemplateType )
    -> StaticHttp.Request StaticData
staticData siteMetadata =
    StaticHttp.succeed ()


canonicalUrl : String
canonicalUrl =
    "https://elm-pages.com"


manifest : Manifest.Config Pages.PathKey
manifest =
    { backgroundColor = Just Color.white
    , categories = [ Pages.Manifest.Category.education ]
    , displayMode = Manifest.Standalone
    , orientation = Manifest.Portrait
    , description = "elm-pages - A statically typed site generator."
    , iarcRatingId = Nothing
    , name = "elm-pages docs"
    , themeColor = Just Color.white
    , startUrl = pages.index
    , shortName = Just "elm-pages"
    , sourceIcon = images.iconPng
    , icons = []
    }


tagline : String
tagline =
    "A statically typed site generator - elm-pages"


webp : MimeType.MimeImage
webp =
    MimeType.OtherImage "webp"

