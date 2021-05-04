module Cloudinary exposing (url)

url :
    String
    -> String
    -> String
url asset transforms =
    let
        base =
            "https://res.cloudinary.com/rolojf/image/upload"
    in
    (base ++ "/" ++ transforms ++ "/" ++ asset)

