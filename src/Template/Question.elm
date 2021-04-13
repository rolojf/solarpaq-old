module Template.Question exposing (Model, Msg, decoder, template)

import Css
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Attributes.Aria as Aria
import Html.Styled as Htmls exposing (div)
import Html.Styled.Attributes as Attr exposing (class)
import Html.Styled.Events as Events
import Json.Decode as Decode
import Pages exposing (images)
import Pages.PagePath as PagePath exposing (PagePath)
import Pages.StaticHttp as StaticHttp
import Shared exposing (UsuarioStatus)
import Site
import Tailwind.Breakpoints as TwBp
import Tailwind.Utilities as Tw
import Task
import Template exposing (StaticPayload, TemplateWithState)
import TemplateMetadata exposing (Question)
import TemplateType exposing (TemplateType)


type alias StaticData =
    ()


type alias Model =
    { usuarioStatus : UsuarioStatus
    , nombre : String
    , comoSupo : String
    , correo : String
    , comentario : String
    , telefono : String
    , apellido : String
    , listo : Bool
    }


type Msg
    = Nombre String
    | ComoSupo String
    | Correo String
    | Apellido String
    | Telefono String
    | Comentario String
    | Enviado
    | Respondio Bool


template : TemplateWithState Question StaticData Model Msg
template =
    Template.noStaticData { head = head }
        |> Template.buildWithSharedState
            { view = view
            , init = init
            , update = update
            , subscriptions = \_ _ _ _ -> Sub.none
            }


init : Question -> ( Model, Cmd Msg )
init metadata =
    ( { usuarioStatus = Shared.Desconocido
      , nombre = ""
      , comoSupo = ""
      , correo = ""
      , comentario = ""
      , telefono = ""
      , apellido = ""
      , listo = False
      }
    , Cmd.none
    )


update : Question -> Msg -> Model -> Shared.Model -> ( Model, Cmd Msg, Maybe Shared.SharedMsg )
update metadata msg model sharedModel =
    case msg of
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

        Comentario cCampo ->
            ( { model | comentario = cCampo }, Cmd.none, Nothing )

        Enviado ->
            ( { model | listo = True }
            , Cmd.none
            , Nothing
            )

        Respondio conQue ->
            ( { model | usuarioStatus =
                if conQue then
                    Shared.Conocido
                else
                    Shared.Rechazado
                 }
            , Cmd.none
            , Nothing
            )


staticData :
    List ( PagePath Pages.PathKey, TemplateType )
    -> StaticHttp.Request StaticData
staticData siteMetadata =
    StaticHttp.succeed ()


decoder : Decode.Decoder Question
decoder =
    Decode.map2 Question
        (Decode.field "title" Decode.string)
        (Decode.succeed
            -- definir aquí como va el menú
            [ { direccion = "veinte"
              , queDice = "twenty"
              }
            , { direccion = "treinta"
              , queDice = "thirty"
              }
            ]
        )


head : StaticPayload Question StaticData -> List (Head.Tag Pages.PathKey)
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
    -> StaticPayload Question StaticData
    -> Shared.RenderedBody
    -> Shared.PageView Msg
view model sharedModel allMetadata staticPayload rendered =
    { title = staticPayload.metadata.title
    , body =
        [ div
            [ class "columna cero" ]
            [ Htmls.text "Aquí esta la documentación de prueba"
            , div [ Attr.css [ Tw.prose, TwBp.lg [ Tw.prose_xl ] ] ]
                (Tuple.second rendered
                    |> List.map (Html.map never)
                    |> List.map Htmls.fromUnstyled
                )
            ]
        , div
            [ Attr.css
                [ Tw.max_w_7xl
                , Tw.mx_auto
                , TwBp.sm [ Tw.px_6 ]
                , TwBp.lg [ Tw.px_8 ]
                ]
            ]
            [ div
                [ Attr.css [ Tw.relative, Tw.bg_white ] ]
                [ viewLayout
                , if not model.listo then
                    viewFormulario model

                  else
                    div
                        [ Attr.css [ TwBp.lg [ Tw.h_72 ]] ]
                        [ Htmls.text <|
                           case model.usuarioStatus of
                                Shared.Desconocido -> "Vamos a confirmar pues!"
                                Shared.Conocido -> "HEY! Sos Familia!!"
                                Shared.Rechazado -> "Pareces un Bot, intenta de nuevo"
                                _ -> "¿Cómo chingados llegué a este estado?"
                        , Htmls.node "el-reto"
                                      [ alResponder Respondio ]
                                      []
                        ]
                ]
            ]
        ]
            |> List.map Htmls.toUnstyled
    }


viewLayout : Htmls.Html Msg
viewLayout =
    div
        [ Attr.css [ TwBp.lg [ Tw.absolute, Tw.inset_0 ] ] ]
        [ div
            [ Attr.css [ TwBp.lg [ Tw.absolute, Tw.inset_y_0, Tw.right_0, Tw.w_1over2 ] ] ]
            [ Htmls.img
                [ Attr.css
                    [ Tw.h_56
                    , Tw.w_full
                    , Tw.object_cover
                    , TwBp.lg [ Tw.absolute, Tw.h_full ]
                    ]
                , Attr.src "https://images.unsplash.com/photo-1556761175-4b46a572b786?ixlib=rb-1.2.1&ixqx=g09zpRVLoT&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=crop&w=1567&q=80"
                , Attr.alt ""
                ]
                []
            ]
        ]


viewFormulario : Model -> Htmls.Html Msg
viewFormulario model =
    let
        viewCampoNombre =
            div
                []
                [ Htmls.label
                    [ Attr.for "first_name"
                    , Attr.css
                        [ Tw.block
                        , Tw.text_sm
                        , Tw.font_medium
                        , Tw.text_gray_700
                        ]
                    ]
                    [ Htmls.text "First name" ]
                , div
                    [ Attr.css [ Tw.mt_1 ] ]
                    [ Htmls.input
                        [ Attr.type_ "text"
                        , Attr.name "first_name"
                        , Attr.id "first_name"
                        , Attr.required True
                        , Attr.minlength 2
                        , Attr.maxlength 15
                        , Attr.autocomplete True -- "given-name"
                        , Attr.css
                            [ Tw.block
                            , Tw.w_full
                            , Tw.shadow_sm
                            , TwBp.sm [ Tw.text_sm ]
                            , Css.focus
                                [ Tw.ring_indigo_500
                                , Tw.border_indigo_500
                                ]
                            , Tw.border_gray_300
                            , Tw.rounded_md
                            ]
                        , Events.onInput Nombre
                        ]
                        []
                    ]
                ]

        viewCampoApellido =
            div []
                [ Htmls.label
                    [ Attr.for "last_name"
                    , Attr.css
                        [ Tw.block
                        , Tw.text_sm
                        , Tw.font_medium
                        , Tw.text_gray_700
                        ]
                    ]
                    [ Htmls.text "Last name" ]
                , div
                    [ Attr.css [ Tw.mt_1 ] ]
                    [ Htmls.input
                        [ Attr.type_ "text"
                        , Attr.name "last_name"
                        , Attr.id "last_name"
                        , Attr.autocomplete True -- "family-name"
                        , Attr.css
                            [ Tw.block
                            , Tw.w_full
                            , Tw.shadow_sm
                            , TwBp.sm [ Tw.text_sm ]
                            , Css.focus
                                [ Tw.ring_indigo_500
                                , Tw.border_indigo_500
                                ]
                            , Tw.border_gray_300
                            , Tw.rounded_md
                            ]
                        , Events.onInput Apellido
                        ]
                        []
                    ]
                ]

        viewCampoCorreo =
            div
                [ Attr.css [ TwBp.sm [ Tw.col_span_2 ] ] ]
                [ Htmls.label
                    [ Attr.for "email"
                    , Attr.css
                        [ Tw.block
                        , Tw.text_sm
                        , Tw.font_medium
                        , Tw.text_gray_700
                        ]
                    ]
                    [ Htmls.text "Email" ]
                , div
                    [ Attr.css [ Tw.mt_1 ] ]
                    [ Htmls.input
                        [ Attr.id "email"
                        , Attr.name "email"
                        , Attr.type_ "email"
                        , Attr.autocomplete True --"email"
                        , Attr.css
                            [ Tw.block
                            , Tw.w_full
                            , Tw.shadow_sm
                            , TwBp.sm [ Tw.text_sm ]
                            , Css.focus
                                [ Tw.ring_indigo_500
                                , Tw.border_indigo_500
                                ]
                            , Tw.border_gray_300
                            , Tw.rounded_md
                            ]
                        , Events.onInput Correo
                        ]
                        []
                    ]
                ]

        viewCampoTelefono =
            div
                [ Attr.css [ TwBp.sm [ Tw.col_span_2 ] ] ]
                [ div
                    [ Attr.css
                        [ Tw.flex
                        , Tw.justify_between
                        ]
                    ]
                    [ Htmls.label
                        [ Attr.for "phone"
                        , Attr.css
                            [ Tw.block
                            , Tw.text_sm
                            , Tw.font_medium
                            , Tw.text_gray_700
                            ]
                        ]
                        [ Htmls.text "Phone" ]
                    , Htmls.span
                        [ Attr.id "phone_description"
                        , Attr.css
                            [ Tw.text_sm
                            , Tw.text_gray_500
                            ]
                        ]
                        [ Htmls.text "Optional" ]
                    ]
                , div
                    [ Attr.css [ Tw.mt_1 ] ]
                    [ Htmls.input
                        [ Attr.type_ "text"
                        , Attr.name "phone"
                        , Attr.id "phone"
                        , Attr.minlength 8
                        , Attr.maxlength 15
                        , Attr.value model.telefono
                        , Attr.autocomplete True -- "tel"
                        , Aria.ariaDescribedby "phone_description" |> Attr.fromUnstyled
                        , Attr.css
                            [ Tw.block
                            , Tw.w_full
                            , Tw.shadow_sm
                            , TwBp.sm [ Tw.text_sm ]
                            , Css.focus [ Tw.ring_indigo_500, Tw.border_indigo_500 ]
                            , Tw.border_gray_300
                            , Tw.rounded_md
                            ]
                        , Events.onInput Telefono
                        ]
                        []
                    ]
                ]

        viewCampoComment =
            div
                [ Attr.css [ TwBp.sm [ Tw.col_span_2 ] ] ]
                [ div
                    [ Attr.css [ Tw.flex, Tw.justify_between ] ]
                    [ Htmls.label
                        [ Attr.for "how_can_we_help"
                        , Attr.css [ Tw.block, Tw.text_sm, Tw.font_medium, Tw.text_gray_700 ]
                        ]
                        [ Htmls.text "How can we help you?" ]
                    , Htmls.span
                        [ Attr.id "how_can_we_help_description"
                        , Attr.css [ Tw.text_sm, Tw.text_gray_500 ]
                        ]
                        [ Htmls.text ">Max. 500 characters" ]
                    ]
                , div
                    [ Attr.css [ Tw.mt_1 ] ]
                    [ Htmls.textarea
                        [ Attr.id "how_can_we_help"
                        , Attr.name "how_can_we_help"
                        , Aria.ariaDescribedby "how_can_we_help_description" |> Attr.fromUnstyled
                        , Attr.rows 4
                        , Attr.css
                            [ Tw.block
                            , Tw.w_full
                            , Tw.shadow_sm
                            , TwBp.sm [ Tw.text_sm ]
                            , Css.focus [ Tw.ring_indigo_500, Tw.border_indigo_500 ]
                            , Tw.border_gray_300
                            , Tw.rounded_md
                            ]
                        , Events.onInput Comentario
                        ]
                        []
                    ]
                ]

        viewComoSupoDeNos =
            div
                [ Attr.css [ TwBp.sm [ Tw.col_span_2 ] ] ]
                [ Htmls.label
                    [ Attr.for "how_did_you_hear_about_us"
                    , Attr.css [ Tw.block, Tw.text_sm, Tw.font_medium, Tw.text_gray_700 ]
                    ]
                    [ Htmls.text "How did you hear about us?" ]
                , div
                    [ Attr.css [ Tw.mt_1 ] ]
                    [ Htmls.input
                        [ Attr.type_ "text"
                        , Attr.name "how_did_you_hear_about_us"
                        , Attr.id "how_did_you_hear_about_us"
                        , Attr.css
                            [ Tw.shadow_sm
                            , Css.focus [ Tw.ring_indigo_500, Tw.border_indigo_500 ]
                            , Tw.block
                            , Tw.w_full
                            , TwBp.sm [ Tw.text_sm ]
                            , Tw.border_gray_300
                            , Tw.rounded_md
                            ]
                        , Events.onInput ComoSupo
                        ]
                        []
                    ]
                ]

        viewBotonSubmit =
            div
                [ Attr.css [ Tw.text_right, TwBp.sm [ Tw.col_span_2 ] ] ]
                [ Htmls.button
                    [ Attr.type_ "submit"
                    , Attr.css
                        [ Tw.inline_flex
                        , Tw.justify_center
                        , Tw.py_2
                        , Tw.px_4
                        , Tw.border
                        , Tw.border_transparent
                        , Tw.shadow_sm
                        , Tw.text_sm
                        , Tw.font_medium
                        , Tw.rounded_md
                        , Tw.text_white
                        , Tw.bg_indigo_600
                        , Css.hover [ Tw.bg_indigo_700 ]
                        , Css.focus [ Tw.outline_none, Tw.ring_2, Tw.ring_offset_2, Tw.ring_indigo_500 ]
                        ]
                    ]
                    [ Htmls.text "Submit" ]
                ]
    in
    div
        [ Attr.css
            [ Tw.relative
            , Tw.pt_12
            , Tw.pb_16
            , Tw.px_4
            , TwBp.sm [ Tw.pt_16, Tw.px_6 ]
            , TwBp.lg
                [ Tw.px_8
                , Tw.max_w_7xl
                , Tw.mx_auto
                , Tw.grid
                , Tw.grid_cols_2
                ]
            ]
        ]
        [ div
            [ Attr.css [ TwBp.lg [ Tw.pr_8 ] ] ]
            [ div
                [ Attr.css
                    [ Tw.max_w_md
                    , Tw.mx_auto
                    , TwBp.lg [ Tw.mx_0 ]
                    , TwBp.sm [ Tw.max_w_lg ]
                    ]
                ]
                [ Htmls.h2
                    [ Attr.css
                        [ Tw.text_3xl
                        , Tw.font_extrabold
                        , Tw.tracking_tight
                        , TwBp.sm [ Tw.text_4xl ]
                        ]
                    ]
                    [ Htmls.text "Let's work together" ]
                , Htmls.p
                    [ Attr.css
                        [ Tw.mt_4
                        , Tw.text_lg
                        , Tw.text_gray_500
                        , TwBp.sm [ Tw.mt_3 ]
                        ]
                    ]
                    [ Htmls.text "We’d love to hear from you! Send us a message using the form opposite, or email us. We’d love to hear from you! Send us a message using the form opposite, or email us." ]
                , Htmls.form
                    [ Attr.action "#"
                    , Attr.method "POST"
                    , Events.onSubmit Enviado
                    , Attr.css
                        [ Tw.mt_9
                        , Tw.grid
                        , Tw.grid_cols_1
                        , Tw.gap_y_6
                        , TwBp.sm [ Tw.grid_cols_2, Tw.gap_x_8 ]
                        ]
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


alResponder que =
    Decode.at [ "detail", "resultado" ] Decode.bool
        |> Decode.map que
        |> Events.on "responde"

