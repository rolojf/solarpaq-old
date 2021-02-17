module Correo exposing  (Correo(..), setCorreo)

import Email exposing (isValid)

type Correo
    = Correo String
    | CInvalido

setCorreo : String -> Correo
setCorreo capturado =
   if isValid capturado then (Correo capturado) else CInvalido


