module TemplateMetadata exposing (..)

import Data.Author exposing (Author)
import Date exposing (Date)


type alias BlogIndex =
    {}


type alias BlogPost =
    { title : String
    , description : String
    , published : Date
    , author : Author
    , draft : Bool
    }


type alias Page =
    { title : String }

type alias Pagina =
    { title : String
    , description : String
    }