module TemplateMetadata exposing (..)

import Data.Author exposing (Author)
import Date exposing (Date)


type alias Liga =
    { direccion : String
    , queDice : String
    }


type alias BlogIndex =
    {  menu : List Liga }


type alias BlogPost =
    { title : String
    , description : String
    , published : Date
    , author : Author
    , draft : Bool
    , menu : List Liga
    }


type alias Page =
    { title : String
    , menu : List Liga
    }


type alias Pagina =
    { title : String
    , description : String
    , menu : List Liga
    }


type alias SelComp1 =
    { title : String
    , menu : List Liga
    }
