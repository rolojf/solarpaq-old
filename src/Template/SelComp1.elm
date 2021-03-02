module Template.SelComp1 exposing (Model, Msg, decoder, template)

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
import Shared
import Site
import Template exposing (StaticPayload, TemplateWithState)
import TemplateMetadata exposing (SelComp1)
import TemplateType exposing (TemplateType)


type alias StaticData =
    ()


type alias Model =
    { inversorTipo : InversorTipo }


type InversorTipo
    = InversorCentral
    | MicroInversor
    | OtroTipo


type Msg
    = Increment
    | RadioSeleccionado InversorTipo


template : TemplateWithState SelComp1 StaticData Model Msg
template =
    Template.noStaticData { head = head }
        |> Template.buildWithSharedState
            { view = view
            , init = init
            , update = update
            , subscriptions = \_ _ _ _ -> Sub.none
            }


init : SelComp1 -> ( Model, Cmd Msg )
init metadata =
    ( { inversorTipo = InversorCentral }, Cmd.none )


update : SelComp1 -> Msg -> Model -> Shared.Model -> ( Model, Cmd Msg, Maybe Shared.SharedMsg )
update metadata msg model sharedModel =
    case msg of
        Increment ->
            ( model, Cmd.none, Just Shared.IncrementFromChild )

        RadioSeleccionado inversorTipo ->
            ( { model | inversorTipo = inversorTipo }
            , Cmd.none
            , Nothing
            )


staticData :
    List ( PagePath Pages.PathKey, TemplateType )
    -> StaticHttp.Request StaticData
staticData siteMetadata =
    StaticHttp.succeed ()


decoder : Decode.Decoder SelComp1
decoder =
    Decode.map2 SelComp1
        (Decode.field "title" Decode.string)
        (Decode.succeed
            -- definir aquí como va el menú
            [ { direccion = "ocho"
              , queDice = "Eight"
              }
            , { direccion = "nueve"
              , queDice = "Nine"
              }
            ]
        )


head : StaticPayload SelComp1 StaticData -> List (Head.Tag Pages.PathKey)
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
    -> StaticPayload SelComp1 StaticData
    -> Shared.RenderedBody
    -> Shared.PageView Msg
view model sharedModel allMetadata staticPayload rendered =
    { title = staticPayload.metadata.title
    , body =
        [ div
            [ class "min-h-3/4 bg-gray-100" ]
            [ div
                [ class "py-6" ]
                [ div
                    [ class "max-w-3xl mx-auto sm:px-6 lg:max-w-7xl lg:px-8 lg:grid lg:grid-cols-12 lg:gap-8" ]
                    [ div
                        [ class "hidden lg:block lg:col-span-3" ]
                        [ Html.section
                            [ class "lg:min-h-full bg-gray-200"]
                            [ div
                                [ Aria.ariaLabel "Sidebar"
                                , class "sticky top-6 divide-y divide-gray-400"
                                ]
                                [ counterView sharedModel
                                , Html.text "Mi Contenido en SideBar"
                                ]
                            ]
                        ]
                    , Html.section
                        [ class "lg:col-span-9" ]
                        ( viewRadioSet model.inversorTipo
                            :: (Tuple.second rendered
                                    |> List.map (Html.map never)
                               )
                        )
                    ]
                ]
            ]
        ]
    }


counterView : Shared.Model -> Html Msg
counterView sharedModel =
    Html.text <| "Docs count: " ++ String.fromInt sharedModel.counter


viewRadioSet : InversorTipo -> Html Msg
viewRadioSet cualInversor =
    let
        opcionesPalRadio =
            [ { descripcion = "Inversor Central"
              , nota = "Optimiza Series de mismo tipo y orientación sin sombras"
              , name = "inversores"
              , id = "central"
              , estado = cualInversor == InversorCentral
              , mensajeAlSeleccionar = RadioSeleccionado InversorCentral
              , extremo = True
              }
            , { descripcion = "micro inversor"
              , nota = "Optimiza cada panel, para diferentes tiposm orientaciones y/o c/sombra"
              , name = "inversores"
              , id = "micros"
              , estado = cualInversor == MicroInversor
              , mensajeAlSeleccionar = RadioSeleccionado MicroInversor
              , extremo = False
              }
            , { descripcion = "Otra Opcion"
              , nota = "Para un futuro matón donde todo jale solo"
              , name = "inversores"
              , id = "otra"
              , estado = cualInversor == OtroTipo
              , mensajeAlSeleccionar = RadioSeleccionado OtroTipo
              , extremo = True
              }
            ]
    in
    Html.fieldset []
        [ Html.legend
            [ class "sr-only" ]
            [ Html.text "Seleccion Inversores" ]
        , div
            [ class "bg-white rounded-md -space-y-px max-w-md" ]
            (opcionesPalRadio
                |> List.map viewRadioOption
            )
        ]


viewRadioOption :
    { descripcion : String
    , nota : String
    , name : String
    , id : String
    , estado : Bool
    , mensajeAlSeleccionar : Msg
    , extremo : Bool
    }
    -> Html Msg
viewRadioOption radioOp =
    let
        claseSegunSeleccion : String
        claseSegunSeleccion =
            case ( radioOp.estado, radioOp.extremo ) of
                ( True, False ) ->
                    "relative border bg-indigo-50 border-indigo-200 z-10 rounded-bl-md rounded-br-md p-4 flex"

                ( False, False ) ->
                    "relative border border-gray-200 p-4 flex"

                ( True, True ) ->
                    "relative border bg-indigo-50 border-indigo-200 z-10 rounded-bl-md rounded-br-md p-4 flex"

                ( False, True ) ->
                    "relative border border-gray-200 rounded-bl-md rounded-br-md p-4 flex"
    in
    div
        [ class claseSegunSeleccion ]
        [ div
            [ class "flex items-center h-5" ]
            [ Html.input
                [ Attr.id radioOp.id
                , Attr.name radioOp.name
                , Attr.type_ "radio"
                , Html.Events.onCheck (\_ -> radioOp.mensajeAlSeleccionar)
                , class "focus:ring-indigo-500 h-4 w-4 text-indigo-600 cursor-pointer border-gray-300"
                ]
                []
            ]
        , Html.label
            [ Attr.for radioOp.id
            , class "ml-3 flex flex-col cursor-pointer"
            ]
            [ Html.span
                [ class <|
                    if radioOp.estado then
                        "text-indigo-900"

                    else
                        "text-gray-900"
                ]
                [ Html.span
                    [ class "block text-sm font-medium" ]
                    [ Html.text radioOp.descripcion ]
                , Html.span
                    [ class <|
                        if radioOp.estado then
                            "text-indigo-700"

                        else
                            "text-gray-500"
                    , class "block text-sm"
                    ]
                    [ Html.text radioOp.nota ]
                ]
            ]
        ]
