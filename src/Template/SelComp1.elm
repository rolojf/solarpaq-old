module Template.SelComp1 exposing (Model, Msg, decoder, template)

import DropList
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Styled as Htmls exposing (div)
import Html.Styled.Attributes as Attr exposing (class)
import Html.Styled.Attributes.Aria as Aria
import Json.Decode as Decode
import OptimizedDecoder as D
import OptionRadio
import Pages exposing (images)
import Pages.PagePath as PagePath exposing (PagePath)
import Pages.StaticHttp as StaticHttp
import Platform.Cmd
import Secrets
import Shared
import Site
import Tailwind.Breakpoints as TwBp
import Tailwind.Utilities as Tw
import Template exposing (StaticPayload, TemplateWithState)
import TemplateMetadata exposing (SelComp1)
import TemplateType exposing (TemplateType)
import Url


type alias Model =
    { tipoDeInversorEscogido : OptionRadio.Model InversorTipo
    , dropList : DropList.Model
    }


type InversorTipo
    = InversorCentral
    | MicroInversor
    | OtroTipo


type Msg
    = Increment
    | RadioSeleccionado InversorTipo
    | DropList DropList.Msg
    | NoOp


template : TemplateWithState SelComp1 StaticData Model Msg
template =
    Template.withStaticData
        { head = head
        , staticData = staticData
        }
        |> Template.buildWithSharedState
            { view = view
            , init = init
            , update = update
            , subscriptions = \_ _ _ _ -> Sub.none
            }


init : SelComp1 -> ( Model, Cmd Msg )
init metadata =
    ( { tipoDeInversorEscogido = OptionRadio.init InversorCentral
      , dropList = DropList.init allOptions
      }
    , Cmd.none
    )


update : SelComp1 -> Msg -> Model -> Shared.Model -> ( Model, Cmd Msg, Maybe Shared.SharedMsg )
update metadata msg model sharedModel =
    case msg of
        Increment ->
            ( model, Cmd.none, Just Shared.IncrementFromChild )

        RadioSeleccionado cualInversor ->
            ( { model
                | tipoDeInversorEscogido =
                    OptionRadio.update cualInversor model.tipoDeInversorEscogido
              }
            , Cmd.none
            , Nothing
            )

        DropList ddModelo ->
            let
                ( modeloRegresa, comando, msgCompartido ) =
                    DropList.update ddModelo model.dropList
            in
            ( { model | dropList = modeloRegresa }
            , comando |> Platform.Cmd.map DropList
            , msgCompartido
            )

        _ ->
            ( model, Cmd.none, Nothing )


type alias StaticData =
    { panelSolar : List DropList.Option }


panelDecoder : D.Decoder StaticData
panelDecoder =
    let
        meteAlRegistro losPaneles =
            { panelSolar = losPaneles }
    in
    D.map2 (::)
        panelDecoderDefaultVariant
        panelDecoderOtherVariant
        |> D.map meteAlRegistro


panelDecoderOtherVariant =
    let
        resultadoSanityOtherProductVariant =
            D.list otherVariants
                |> D.field "variants"

        otherVariants =
            D.map2 (\miId miLabel -> { id = miId, label = miLabel })
                (D.field "sku" D.string)
                (D.field "title" D.string)

        tomaLaCabeza listado =
            List.head listado
                |> Maybe.withDefault
                    [ { id = "pu"
                      , label = "Plutonium"
                      }
                    ]
    in
    D.list resultadoSanityOtherProductVariant
        |> D.field "result"
        |> D.map tomaLaCabeza


panelDecoderDefaultVariant =
    let
        resultadoSanityDefaultProductVariant =
            D.map2 (\miId miLabel -> { id = miId, label = miLabel })
                (D.at [ "defaultProductVariant", "sku" ] D.string)
                (D.at [ "defaultProductVariant", "title" ] D.string)

        tomaLaCabeza listado =
            List.head listado
                |> Maybe.withDefault
                    { id = "pu"
                    , label = "Plutonium"
                    }
    in
    D.list resultadoSanityDefaultProductVariant
        |> D.field "result"
        |> D.map tomaLaCabeza


staticData :
    List ( PagePath Pages.PathKey, TemplateType )
    -> StaticHttp.Request StaticData
staticData siteMetadata =
    let
        detalles =
            Secrets.succeed
                (\proyId ->
                    { url =
                        "https://"
                            ++ proyId
                            ++ ".api.sanity.io/v1/data/query/production?query="
                            ++ Url.percentEncode
                                """*[_type == "product" && title == "Panel Solar"]"""
                    , method = "GET"
                    , headers = []
                    , body = StaticHttp.emptyBody
                    }
                )
                |> Secrets.with "SANITY_PROJECT_ID"
    in
    StaticHttp.request detalles panelDecoder


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
            [ Attr.css [ Tw.min_h_3over4, Tw.bg_gray_100 ] ]
            [ div
                [ Attr.css [ Tw.py_6 ] ]
                [ div
                    [ Attr.css
                        [ Tw.max_w_3xl
                        , Tw.mx_auto
                        , TwBp.lg [ Tw.max_w_7xl, Tw.px_8, Tw.grid, Tw.grid_cols_12, Tw.gap_8 ]
                        , TwBp.sm [ Tw.px_6 ]
                        ]
                    ]
                    [ div
                        [ Attr.css
                            [ Tw.hidden
                            , TwBp.lg [ Tw.block, Tw.col_span_3 ]
                            ]
                        ]
                        [ Htmls.section
                            [ Attr.css [ TwBp.lg [ Tw.min_h_full, Tw.bg_gray_200 ] ] ]
                            [ div
                                [ Aria.ariaLabel "Sidebar"
                                , Attr.css [ Tw.sticky, Tw.top_6, Tw.divide_y, Tw.divide_gray_400 ]
                                ]
                                [ counterView sharedModel
                                , Htmls.br [] []
                                , Htmls.text "Mi Contenido en SideBar"
                                ]
                            ]
                        ]
                    , Htmls.section
                        [ Attr.css [ TwBp.lg [ Tw.col_span_9 ] ] ]
                        [ OptionRadio.view
                            "Seleccion Inversores"
                            model.tipoDeInversorEscogido
                            opcionesPalRadio
                            |> Htmls.map RadioSeleccionado
                        , DropList.view
                            (actualizaDropList
                                model.dropList
                                staticPayload.static.panelSolar
                            )
                            |> Htmls.map DropList
                        ]
                    , div
                        [ Attr.css [ Tw.mt_4, Tw.text_xl, Tw.text_center ] ]
                        (Tuple.second rendered
                            |> List.map (Html.map never)
                            |> List.map Htmls.fromUnstyled
                        )
                    ]
                ]
            ]
            |> Htmls.toUnstyled
        ]
    }


actualizaDropList : DropList.Model -> List DropList.Option -> DropList.Model
actualizaDropList model opciones =
    { model | options = opciones }


allOptions : List DropList.Option
allOptions =
    [ { id = "Np", label = "Neptunium" }
    , { id = "Pu", label = "Plutonium" }
    , { id = "Am", label = "Americium" }
    , { id = "Cm", label = "Curium" }
    , { id = "Bk", label = "Berkelium" }
    , { id = "Cf", label = "Californium" }
    , { id = "Fm", label = "Fermium" }
    , { id = "Md", label = "Mendelevium" }
    , { id = "No", label = "Nobelium" }
    , { id = "Lr", label = "Lawrencium" }
    , { id = "Rf", label = "Rutherfordium" }
    , { id = "Db", label = "Dubnium" }
    , { id = "Sg", label = "Seaborgium" }
    , { id = "Bh", label = "Bohrium" }
    , { id = "Hs", label = "Hassium" }
    ]


opcionesPalRadio : List (OptionRadio.Option InversorTipo)
opcionesPalRadio =
    [ { descripcion = "Inversor Central"
      , nota = "Optimiza Series de mismo tipo y orientación sin sombras"
      , name = "inversores"
      , id = "central"
      , tipo = InversorCentral
      , extremo = True
      }
    , { descripcion = "micro inversor"
      , nota = "Optimiza cada panel, para diferentes tiposm orientaciones y/o c/sombra"
      , name = "inversores"
      , id = "micros"
      , tipo = MicroInversor
      , extremo = False
      }
    , { descripcion = "Otra Opcion"
      , nota = "Para un futuro matón donde todo jale solo"
      , name = "inversores"
      , id = "otra"
      , tipo = OtroTipo
      , extremo = True
      }
    ]


counterView : Shared.Model -> Htmls.Html Msg
counterView sharedModel =
    Htmls.text <| "Docs count: " ++ String.fromInt sharedModel.counter
