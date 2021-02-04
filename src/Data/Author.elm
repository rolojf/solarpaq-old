module Data.Author exposing (Author, all, decoder, view)

import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class)
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Pages
import Pages.ImagePath as ImagePath exposing (ImagePath)


type alias Author =
    { name : String
    , avatar : ImagePath Pages.PathKey
    , bio : String
    }


all : List Author
all =
    [ { name = "Dillon Kearns"
      , avatar = Pages.images.author.dillon
      , bio = "Elm developer and educator. Founder of Incremental Elm Consulting."
      }
    ]


decoder : Decoder Author
decoder =
    Decode.string
        |> Decode.andThen
            (\lookupName ->
                case List.Extra.find (\currentAuthor -> currentAuthor.name == lookupName) all of
                    Just author ->
                        Decode.succeed author

                    Nothing ->
                        Decode.fail ("Couldn't find author with name " ++ lookupName ++ ". Options are " ++ String.join ", " (List.map .name all))
            )


view : List (Html.Attribute msg) -> Author -> Html msg
view attributes author =
    Html.img
        [ class "avatar w-16 h-16 rounded-full clase-en-author-def"
        , Attr.src <| ImagePath.toString author.avatar
        , Attr.alt author.name
        ] []
