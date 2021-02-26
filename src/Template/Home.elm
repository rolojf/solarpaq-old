module Template.Home exposing (Model, Msg, decoder, template)

import Browser.Dom as Dom
import Head
import Head.Seo as Seo
import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class)
import Html.Attributes.Aria as Aria
import Html.Events
import Json.Decode as Decode
import MarkdownRenderer
import Pages exposing (images)
import Pages.PagePath as PagePath exposing (PagePath)
import Pages.StaticHttp as StaticHttp
import Palette
import Process
import Shared exposing (UsuarioStatus)
import Site
import Task
import Template exposing (StaticPayload, TemplateWithState)
import TemplateMetadata exposing (Home)
import TemplateType exposing (TemplateType)


type alias StaticData =
    ()


type alias Model =
    { intentos : Int
    , usuarioStatus : UsuarioStatus
    , nombre : String
    , comoSupo : String
    , correo : String
    , comentario : String
    , telefono : String
    , apellido : String
    , respondioBien : Bool
    , listo : Bool
    , nuevoIntento : Intentos
    , queRespondio : String
    }


type Msg
    = Increment
    | Nombre String
    | ComoSupo String
    | Correo String
    | Apellido String
    | Telefono String
    | Respondio String
    | Comentario String
    | Enviado
    | IntentaDeNuez
    | ResultaDelFocus


template : TemplateWithState Home StaticData Model Msg
template =
    Template.noStaticData { head = head }
        |> Template.buildWithSharedState
            { view = view
            , init = init
            , update = update
            , subscriptions = \_ _ _ _ -> Sub.none
            }


init : Home -> ( Model, Cmd Msg )
init metadata =
    ( { intentos = 0
      , usuarioStatus = Shared.Desconocido
      , nombre = ""
      , comoSupo = ""
      , correo = ""
      , comentario = ""
      , telefono = ""
      , apellido = ""
      , respondioBien = False
      , listo = False
      , nuevoIntento = VaPues
      , queRespondio = ""
      }
    , Cmd.none
    )


type Intentos
    = VaPues
    | YaRespondio
    | VaDeNuevo
    | YaOk


update : Home -> Msg -> Model -> Shared.Model -> ( Model, Cmd Msg, Maybe Shared.SharedMsg )
update metadata msg model sharedModel =
    case msg of
        Increment ->
            ( model, Cmd.none, Just Shared.IncrementFromChild )

        Nombre cCampo ->
            ( { model | nombre = cCampo }, Cmd.none, Nothing )

        ComoSupo cCampo ->
            ( { model | comoSupo = cCampo }, Cmd.none, Nothing )

        Correo cCampo ->
            ( { model | correo = cCampo }, Cmd.none, Nothing )

        Apellido cCampo ->
            ( { model | apellido = cCampo }, Cmd.none, Nothing )

        Telefono conQue ->
            let
                entered =
                    String.right 1 conQue

                conQue1 =
                    if String.contains entered "01234567890 _-.+" then
                        entered

                    else
                        ""
            in
            ( { model | telefono = String.dropRight 1 conQue ++ conQue1 }, Cmd.none, Nothing )

        Respondio conQue ->
            let
                seLaSupo =
                    if conQue == "4" then
                        True

                    else
                        False
            in
            ( { model
                | respondioBien = seLaSupo
                , nuevoIntento =
                    if seLaSupo then
                        YaOk

                    else
                        YaRespondio
                , queRespondio = conQue
              }
            , if seLaSupo then
                Task.perform (\_ -> IntentaDeNuez) <| Process.sleep 600

              else
                Task.perform (\_ -> IntentaDeNuez) <| Process.sleep 500
            , Nothing
            )

        Comentario cCampo ->
            ( { model | comentario = cCampo }, Cmd.none, Nothing )

        Enviado ->
            ( { model | listo = True }
            , Dom.focus "valor-challenge"
                |> Task.attempt (\_ -> ResultaDelFocus)
            , Nothing
            )

        IntentaDeNuez ->
            ( { model
                | intentos = model.intentos + 1
                , nuevoIntento = VaPues
                , queRespondio = ""
                , usuarioStatus =
                    case ( model.respondioBien, model.usuarioStatus ) of
                        ( False, Shared.Desconocido ) ->
                            if model.intentos >= 3 then
                                Shared.Rechazado

                            else
                                Shared.Desconocido

                        ( False, Shared.Conocido ) ->
                            Shared.Conocido

                        ( False, Shared.DiceQueRegresa ) ->
                            Shared.DiceQueRegresa

                        ( False, Shared.Rechazado ) ->
                            Shared.Rechazado

                        ( True, Shared.Desconocido ) ->
                            Shared.Conocido

                        ( True, Shared.Rechazado ) ->
                            Shared.Rechazado

                        ( True, Shared.Conocido ) ->
                            Shared.Conocido

                        ( True, Shared.DiceQueRegresa ) ->
                            Shared.DiceQueRegresa
              }
            , Cmd.none
            , Nothing
            )

        ResultaDelFocus ->
            ( model, Cmd.none, Nothing )


staticData :
    List ( PagePath Pages.PathKey, TemplateType )
    -> StaticHttp.Request StaticData
staticData siteMetadata =
    StaticHttp.succeed ()


decoder : Decode.Decoder Home
decoder =
    Decode.map2 Home
        (Decode.field "title" Decode.string)
        (Decode.succeed
            -- definir aquí como va el menú
            [ { direccion = "once"
              , queDice = "eleven"
              }
            , { direccion = "doce"
              , queDice = "Twuelve"
              }
            ]
        )


head : StaticPayload Home StaticData -> List (Head.Tag Pages.PathKey)
head staticPayload =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = images.iconPng
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = Site.tagline
        , locale = Nothing
        , title = staticPayload.metadata.title
        }
        |> Seo.website


view :
    Model
    -> Shared.Model
    -> List ( PagePath Pages.PathKey, TemplateType )
    -> StaticPayload Home StaticData
    -> Shared.RenderedBody
    -> Shared.PageView Msg
view model sharedModel allMetadata staticPayload rendered =
    { title = staticPayload.metadata.title
    , body =
        [ div [ class "columna cero" ]
            [ counterView sharedModel
            , Html.text "Aquí estaba el listado de la documentación"
            , div [ class "primera columna" ]
                [ Palette.heading 1 [ Html.text staticPayload.metadata.title ]
                , div [ class "segunda columna" ]
                    [ tocView staticPayload.path (Tuple.first rendered)
                    , div
                        [ class "tercer columna" ]
                        (Tuple.second rendered |> List.map (Html.map never))
                    ]
                ]
            , div
                [ class "max-w-7xl mx-auto sm:px-6 lg:px-8" ]
                [ viewFormulario model
                , case ( model.listo, model.usuarioStatus ) of
                    ( True, Shared.Desconocido ) ->
                        viewChallenge model

                    ( True, Shared.Rechazado ) ->
                        Html.text "Shame on you!"

                    ( True, Shared.Conocido ) ->
                        Html.text "Wellcome!"

                    ( True, Shared.DiceQueRegresa ) ->
                        Html.text "Ya veremos..."

                    ( False, _ ) ->
                        Html.text ""
                ]
            ]
        ]
    }


counterView : Shared.Model -> Html Msg
counterView sharedModel =
    Html.text <| "Docs count: " ++ String.fromInt sharedModel.counter


tocView : PagePath Pages.PathKey -> MarkdownRenderer.TableOfContents -> Html msg
tocView path toc =
    div [ class "toc uno" ]
        [ Html.text "Table of Contents"
        , div [ class "toc dos" ]
            (toc
                |> List.map
                    (\heading ->
                        Html.a
                            [ Attr.href
                                (PagePath.toString path ++ "#" ++ heading.anchorId)
                            , class "clase de ligas"
                            ]
                            [ Html.text heading.name ]
                    )
            )
        ]


viewFormulario : Model -> Html Msg
viewFormulario model =
    let
        viewCampoNombre =
            div
                []
                [ Html.label
                    [ Attr.for "first_name"
                    , class "block text-sm font-medium text-gray-700"
                    ]
                    [ Html.text "First name" ]
                , div
                    [ class "mt-1" ]
                    [ Html.input
                        [ Attr.type_ "text"
                        , Attr.name "first_name"
                        , Attr.id "first_name"
                        , Attr.required True
                        , Attr.minlength 2
                        , Attr.maxlength 15
                        , Attr.autocomplete True -- "given-name"
                        , class "block w-full shadow-sm sm:text-sm focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 rounded-md"
                        , Html.Events.onInput Nombre
                        ]
                        []
                    ]
                ]

        viewCampoApellido =
            div []
                [ Html.label
                    [ Attr.for "last_name"
                    , class "block text-sm font-medium text-gray-700"
                    ]
                    [ Html.text "Last name" ]
                , div
                    [ class "mt-1" ]
                    [ Html.input
                        [ Attr.type_ "text"
                        , Attr.name "last_name"
                        , Attr.id "last_name"
                        , Attr.autocomplete True -- "family-name"
                        , class "block w-full shadow-sm sm:text-sm focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 rounded-md"
                        , Html.Events.onInput Apellido
                        ]
                        []
                    ]
                ]

        viewCampoCorreo =
            div
                [ class "sm:col-span-2" ]
                [ Html.label
                    [ Attr.for "email"
                    , class "block text-sm font-medium text-gray-700"
                    ]
                    [ Html.text "Email" ]
                , div
                    [ class "mt-1" ]
                    [ Html.input
                        [ Attr.id "email"
                        , Attr.name "email"
                        , Attr.type_ "email"
                        , Attr.autocomplete True --"email"
                        , class "block w-full shadow-sm sm:text-sm focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 rounded-md"
                        , Html.Events.onInput Correo
                        ]
                        []
                    ]
                ]

        viewCampoTelefono =
            div
                [ class "sm:col-span-2" ]
                [ div
                    [ class "flex justify-between" ]
                    [ Html.label
                        [ Attr.for "phone"
                        , class "block text-sm font-medium text-gray-700"
                        ]
                        [ Html.text "Phone" ]
                    , Html.span
                        [ Attr.id "phone_description"
                        , class "text-sm text-gray-500"
                        ]
                        [ Html.text "Optional" ]
                    ]
                , div
                    [ class "mt-1" ]
                    [ Html.input
                        [ Attr.type_ "text"
                        , Attr.name "phone"
                        , Attr.id "phone"
                        , Attr.minlength 8
                        , Attr.maxlength 15
                        , Attr.value model.telefono
                        , Attr.autocomplete True -- "tel"
                        , Aria.ariaDescribedby "phone_description"
                        , class "block w-full shadow-sm sm:text-sm focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 rounded-md"
                        , Html.Events.onInput Telefono
                        ]
                        []
                    ]
                ]

        viewCampoComment =
            div
                [ class "sm:col-span-2" ]
                [ div
                    [ class "flex justify-between" ]
                    [ Html.label
                        [ Attr.for "how_can_we_help"
                        , class "block text-sm font-medium text-gray-700"
                        ]
                        [ Html.text "How can we help you?" ]
                    , Html.span
                        [ Attr.id "how_can_we_help_description"
                        , class "text-sm text-gray-500"
                        ]
                        [ Html.text ">Max. 500 characters" ]
                    ]
                , div
                    [ class "mt-1" ]
                    [ Html.textarea
                        [ Attr.id "how_can_we_help"
                        , Attr.name "how_can_we_help"
                        , Aria.ariaDescribedby "how_can_we_help_description"
                        , Attr.rows 4
                        , class "block w-full shadow-sm sm:text-sm focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 rounded-md"
                        , Html.Events.onInput Comentario
                        ]
                        []
                    ]
                ]

        viewComoSupoDeNos =
            div
                [ class "sm:col-span-2" ]
                [ Html.label
                    [ Attr.for "how_did_you_hear_about_us"
                    , class "block text-sm font-medium text-gray-700"
                    ]
                    [ Html.text "How did you hear about us?" ]
                , div
                    [ class "mt-1" ]
                    [ Html.input
                        [ Attr.type_ "text"
                        , Attr.name "how_did_you_hear_about_us"
                        , Attr.id "how_did_you_hear_about_us"
                        , class "shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w-full sm:text-sm border-gray-300 rounded-md"
                        , Html.Events.onInput ComoSupo
                        ]
                        []
                    ]
                ]

        viewBotonSubmit =
            div
                [ class "text-right sm:col-span-2" ]
                [ Html.button
                    [ Attr.type_ "submit"
                    , class "inline-flex justify-center py-2 px-4 border border-transparent shadow-sm text-sm font-medium rounded-md text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                    ]
                    [ Html.text "Submit" ]
                ]
    in
    div
        [ class "relative bg-white" ]
        [ div
            [ class "lg:absolute lg:inset-0" ]
            [ div
                [ class "lg:absolute lg:inset-y-0 lg:right-0 lg:w-1/2" ]
                [ Html.img
                    [ class "h-56 w-full object-cover lg:absolute lg:h-full"
                    , Attr.src "https://images.unsplash.com/photo-1556761175-4b46a572b786?ixlib=rb-1.2.1&ixqx=g09zpRVLoT&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=crop&w=1567&q=80"
                    , Attr.alt ""
                    ]
                    []
                ]
            ]
        , div
            [ class "relative pt-12 pb-16 px-4 sm:pt-16 sm:px-6 lg:px-8 lg:max-w-7xl lg:mx-auto lg:grid lg:grid-cols-2" ]
            [ div
                [ class "lg:pr-8" ]
                [ div
                    [ class "max-w-md mx-auto sm:max-w-lg lg:mx-0" ]
                    [ Html.h2
                        [ class "text-3xl font-extrabold tracking-tight sm:text-4xl" ]
                        [ Html.text "Let's work together" ]
                    , Html.p
                        [ class "mt-4 text-lg text-gray-500 sm:mt-3" ]
                        [ Html.text "We’d love to hear from you! Send us a message using the form opposite, or email us. We’d love to hear from you! Send us a message using the form opposite, or email us." ]
                    , Html.form
                        [ Attr.action "#"
                        , Attr.method "POST"
                        , Html.Events.onSubmit Enviado
                        , class "mt-9 grid grid-cols-1 gap-y-6 sm:grid-cols-2 sm:gap-x-8"
                        ]
                        [ viewCampoNombre
                        , viewCampoApellido
                        , viewCampoCorreo
                        , viewCampoTelefono
                        , viewCampoComment
                        , viewComoSupoDeNos
                        , viewBotonSubmit
                        ]
                    ]
                ]
            ]
        ]


viewChallenge : Model -> Html Msg
viewChallenge model =
    div
        [ class "la-base-modal" ]
        [ div
            [ class <|
                "mm-fondo"
                    ++ (if model.nuevoIntento == YaRespondio then
                            " animate-bounce"

                        else
                            ""
                       )
            ]
            [ Html.h3
                [ class "mm-titulo" ]
                [ Html.text "Validación Rápida" ]
            , Html.p
                [ class "mm-explic" ]
                [ Html.text "Contesta lo siguiente para validar que eres humano y no un bot" ]
            , div
                [ class "mm-fondo-reto" ]
                [ Html.p
                    [ class "mm-reto" ]
                    [ Html.text "Resuleve la siguiente ecuación: " ]
                , div
                    [ class "mm-acomodo-ecuacion" ]
                    [ Html.p
                        []
                        [ Html.text "7 + " ]
                    , Html.label
                        [ class "sr-only"
                        , Attr.for "valor"
                        ]
                        [ Html.text "número" ]
                    , Html.input
                        [ class "mm-campo text-center"
                        , Attr.id "valor-challenge"
                        , case model.nuevoIntento of
                            VaPues ->
                                Attr.placeholder "?"

                            YaRespondio ->
                                Attr.value model.queRespondio

                            VaDeNuevo ->
                                Attr.value model.queRespondio

                            YaOk ->
                                class "animate-ping"
                        , class "form-input"
                        , Html.Events.onInput Respondio
                        ]
                        []
                    , Html.p
                        []
                        [ Html.text "= 11" ]
                    ]
                , if model.intentos >= 1 then
                    Html.p
                        [ class
                            ("text-right mx-4 "
                                ++ (if model.intentos == 1 then
                                        "text-black"

                                    else if model.intentos == 2 then
                                        "text-red-500"

                                    else
                                        "text-red-500 font-bold italic"
                                   )
                            )
                        ]
                        [ Html.text "Intenta de nuevo!" ]

                  else
                    Html.p [] []
                ]
            ]
        ]
