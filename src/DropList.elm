module DropList exposing (..)

import Browser.Dom as Dom
import Css
import Html.Styled as Htmls exposing (div)
import Html.Styled.Attributes as Attr exposing (class)
import Html.Styled.Attributes.Aria as Aria
import Html.Styled.Events as Events
import Json.Decode as Decode
import Shared
import Svg.Styled as Svg
import Svg.Styled.Attributes
import Tailwind.Breakpoints as TwBp
import Tailwind.Utilities as Tw
import Task
import TemplateMetadata exposing (SelComp1)


type alias Model =
    { open : Bool
    , selectedId : Maybe String
    , focusedId : Maybe String
    , options : List Option
    }


init : List Option -> Model
init lasOpciones =
    { open = False
    , selectedId = Nothing
    , focusedId = Nothing
    , options = lasOpciones
    }


type Msg
    = Toggle
    | Close
    | SelectOption String
    | KeyPress KeyPressed
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Shared.SharedMsg )
update msg model =
    case msg of
        Toggle ->
            if model.open then
                ( { model | open = False, focusedId = Nothing }, Cmd.none, Nothing )

            else
                openDropdown model

        Close ->
            ( { model | open = False, focusedId = Nothing }, Cmd.none, Nothing )

        SelectOption id ->
            ( { model | selectedId = Just id, open = False }, Cmd.none, Nothing )

        KeyPress key ->
            if model.open then
                handleKeyWhenOpen model key

            else
                handleKeyWhenClosed model key

        _ ->
            ( model, Cmd.none, Nothing )


type alias Option =
    { id : String
    , label : String
    }


type State
    = Open
    | Closed


handleKeyWhenClosed : Model -> KeyPressed -> ( Model, Cmd Msg, Maybe Shared.SharedMsg )
handleKeyWhenClosed model key =
    if key == Up || key == Down || key == Enter || key == Space then
        openDropdown model

    else
        ( model, Cmd.none, Nothing )


handleKeyWhenOpen : Model -> KeyPressed -> ( Model, Cmd Msg, Maybe Shared.SharedMsg )
handleKeyWhenOpen model key =
    case key of
        Enter ->
            ( { model | selectedId = model.focusedId, open = False }, Cmd.none, Nothing )

        Space ->
            ( { model | selectedId = model.focusedId, open = False }, Cmd.none, Nothing )

        Up ->
            navigateWithKey model (getPrevId model)

        Down ->
            navigateWithKey model (getNextId model)

        Escape ->
            ( { model | open = False, focusedId = Nothing }, Cmd.none, Nothing )

        Other ->
            ( model, Cmd.none, Nothing )


navigateWithKey : Model -> Maybe String -> ( Model, Cmd Msg, Maybe Shared.SharedMsg )
navigateWithKey model nextId =
    ( { model | focusedId = nextId }
    , nextId |> Maybe.map focusOption |> Maybe.withDefault Cmd.none
    , Nothing
    )


firstId =
    List.head >> Maybe.map .id


lastId =
    List.reverse >> firstId


getPrevId : Model -> Maybe String
getPrevId model =
    case model.focusedId of
        Nothing ->
            lastId model.options

        Just id ->
            model.options |> List.map .id |> findPrev id


getNextId : Model -> Maybe String
getNextId model =
    case model.focusedId of
        Nothing ->
            firstId model.options

        Just id ->
            model.options |> List.map .id |> findNext id


openDropdown : Model -> ( Model, Cmd Msg, Maybe Shared.SharedMsg )
openDropdown model =
    let
        focusedId =
            defaultFocused model
    in
    ( { model | open = True, focusedId = focusedId }
    , focusedId
        |> Maybe.map focusOption
        |> Maybe.withDefault Cmd.none
    , Nothing
    )


defaultFocused : Model -> Maybe String
defaultFocused model =
    case model.selectedId of
        Nothing ->
            firstId model.options

        Just id ->
            Just id



-- VIEW DROPDOWN


dropdownElementId =
    "dropdown"


view : Model -> Htmls.Html Msg
view model =
    div [ Attr.css [ Tw.mt_4, Tw.w_96 ] ]
        [ Htmls.label
            [ Attr.id "listbox-label"
            , Attr.css [ Tw.block, Tw.text_sm, Tw.font_medium, Tw.text_gray_700 ]
            ]
            [ Htmls.text "Asignado a:" ]
        , div
            [ Attr.css [ Tw.mt_1, Tw.relative ]
            , Attr.id dropdownElementId
            , Events.preventDefaultOn "keydown" keyDecoder
            , Events.on "focusout" (onFocusOut dropdownElementId)
            ]
            [ Htmls.button
                [ Attr.css
                    [ Tw.relative
                    , Tw.w_full
                    , Tw.bg_white
                    , Tw.border
                    , Tw.border_gray_300
                    , Tw.rounded_md
                    , Tw.shadow_sm
                    , Tw.pl_3
                    , Tw.pr_10
                    , Tw.py_2
                    , Tw.text_left
                    , Tw.cursor_default
                    , Css.focus [ Tw.outline_none, Tw.ring_1, Tw.ring_indigo_500, Tw.border_indigo_500 ]
                    , TwBp.sm [ Tw.text_sm ]
                    ]
                , Aria.ariaHasPopup "listbox"
                , Aria.ariaExpanded <|
                    if model.open then
                        "true"

                    else
                        "false"
                , Aria.ariaLabelledby "listbox-label"
                , Events.onClick Toggle
                ]
                [ div
                    [ Attr.css [ Tw.flex, Tw.items_center ] ]
                    [ Htmls.span
                        [ Aria.ariaLabel "Online"
                        , Attr.css [ Tw.bg_green_400, Tw.flex_shrink_0, Tw.inline_block, Tw.h_2, Tw.w_2, Tw.rounded_full ]
                        ]
                        []
                    , Htmls.span
                        [ Attr.css <|
                            List.append
                                [ Tw.ml_3, Tw.block, Tw.truncate ]
                                (case model.selectedId of
                                    Just _ ->
                                        [ Tw.font_bold ]

                                    Nothing ->
                                        []
                                )
                        ]
                        [ Htmls.text (getButtonText model "Select...") ]
                    ]
                ]
            , if model.open then
                viewList model

              else
                Htmls.span
                    [ Attr.css [ Tw.absolute, Tw.inset_y_0, Tw.right_0, Tw.flex, Tw.items_center, Tw.pr_2, Tw.pointer_events_none ]
                    , Aria.ariaHidden True
                    ]
                    [ heroIconSolidSelector ]
            ]
        ]


heroIconSolidSelector =
    Svg.svg
        [ Svg.Styled.Attributes.class "h-5 w-5 text-gray-400"
        , Svg.Styled.Attributes.viewBox "0 0 20 20"
        , Svg.Styled.Attributes.fill "currentColor"
        ]
        [ Svg.path
            [ Svg.Styled.Attributes.fillRule "evenodd"
            , Svg.Styled.Attributes.d "M10 3a1 1 0 01.707.293l3 3a1 1 0 01-1.414 1.414L10 5.414 7.707 7.707a1 1 0 01-1.414-1.414l3-3A1 1 0 0110 3zm-3.707 9.293a1 1 0 011.414 0L10 14.586l2.293-2.293a1 1 0 011.414 1.414l-3 3a1 1 0 01-1.414 0l-3-3a1 1 0 010-1.414z"
            , Svg.Styled.Attributes.clipRule "evenodd"
            ]
            []
        ]


heroIconSolidCheckMark =
    Svg.svg
        [ Svg.Styled.Attributes.class "h-5 w-5"
        , Svg.Styled.Attributes.viewBox "0 0 20 20"
        , Svg.Styled.Attributes.fill "currentColor"
        ]
        [ Svg.path
            [ Svg.Styled.Attributes.fillRule "evenodd"
            , Svg.Styled.Attributes.d "M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z"
            , Svg.Styled.Attributes.clipRule "evenodd"
            ]
            []
        ]


viewList : Model -> Htmls.Html Msg
viewList model =
    div
        [ Attr.css [ Tw.absolute, Tw.mt_1, Tw.w_full, Tw.rounded_md, Tw.bg_white, Tw.shadow_lg ] ]
        [ Htmls.ul
            [ Attr.css
                [ Tw.max_h_60
                , Tw.rounded_md
                , Tw.py_1
                , Tw.text_base
                , Tw.ring_1
                , Tw.ring_black
                , Tw.ring_opacity_5
                , Tw.overflow_auto
                , Css.focus [ Tw.outline_none ]
                , TwBp.sm [ Tw.text_sm ]
                ]
            , Attr.attribute "role" "listbox"
            , Aria.ariaLabelledby "listbox-label"
            , Aria.ariaActiveDescendant "listbox-item-3"
            , Attr.tabindex -1
            ]
            (model.options
                |> List.map (viewOption model)
            )
        ]


viewOption : Model -> Option -> Htmls.Html Msg
viewOption model option =
    let
        isSelected =
            maybeEqual model.selectedId option.id
    in
    Htmls.li
        [ Attr.attribute "role" "option"
        , Attr.id option.id
        , Attr.tabindex -1
        , Events.onClick (SelectOption option.id)
        , Attr.css [ Tw.text_gray_900, Tw.cursor_default, Tw.select_none, Tw.relative, Tw.py_2, Tw.pl_3, Tw.pr_9 ]
        , Aria.ariaSelected <|
            if isSelected then
                "true"

            else
                "false"
        ]
        [ div
            [ Attr.css [ Tw.flex, Tw.items_center ] ]
            [ Htmls.span
                [ Attr.css [ Tw.bg_green_400, Tw.flex_shrink_0, Tw.inline_block, Tw.h_2, Tw.w_2, Tw.rounded_full ]
                , Aria.ariaLabel "Online"
                ]
                []
            , Htmls.span
                [ Attr.css <|
                    List.append
                        [ Tw.ml_3, Tw.block, Tw.truncate ]
                        (if isSelected then
                            [ Tw.font_semibold ]

                         else if maybeEqual model.focusedId option.id then
                            [ Tw.text_white, Tw.font_normal, Tw.bg_indigo_600 ]

                         else
                            [ Tw.text_gray_900, Tw.text_base ]
                        )
                ]
                [ Htmls.text option.label
                , Htmls.span
                    [ Attr.css [ Tw.sr_only ] ]
                    [ Htmls.text "is Online" ]
                ]
            ]
        , if isSelected then
            Htmls.span
                [ Attr.css [ Tw.absolute, Tw.inset_y_0, Tw.right_0, Tw.flex, Tw.items_center, Tw.pr_4 ]
                , Aria.ariaHidden True
                ]
                [ heroIconSolidCheckMark ]

          else
            div [] []
        ]


maybeEqual : Maybe String -> String -> Bool
maybeEqual maybeId idToCompare =
    maybeId |> Maybe.map (\id -> id == idToCompare) |> Maybe.withDefault False


getButtonText : Model -> String -> String
getButtonText model placeholder =
    case model.selectedId of
        Nothing ->
            placeholder

        Just id ->
            model.options
                |> byId id
                |> Maybe.map .label
                |> Maybe.withDefault placeholder


byId id =
    List.filter (\option -> option.id == id) >> List.head



-- MAIN
-- EVENT DECODERS


onFocusOut : String -> Decode.Decoder Msg
onFocusOut id =
    outsideTarget "relatedTarget" id


outsideTarget : String -> String -> Decode.Decoder Msg
outsideTarget targetName dropdownId =
    Decode.field targetName (isOutsideDropdown dropdownId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed Close

                else
                    Decode.fail "inside dropdown"
            )


isOutsideDropdown : String -> Decode.Decoder Bool
isOutsideDropdown dropdownId =
    Decode.oneOf
        [ Decode.field "id" Decode.string
            |> Decode.andThen
                (\id ->
                    if dropdownId == id then
                        -- found match by id
                        Decode.succeed False

                    else
                        -- try next decoder
                        Decode.fail "check parent node"
                )
        , Decode.lazy (\_ -> isOutsideDropdown dropdownId |> Decode.field "parentNode")

        -- fallback if all previous decoders failed
        , Decode.succeed True
        ]


focusOption : String -> Cmd Msg
focusOption optionId =
    Task.attempt (\_ -> NoOp) (Dom.focus optionId)


keyDecoder : Decode.Decoder ( Msg, Bool )
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.map toKeyPressed
        |> Decode.map
            (\key ->
                ( KeyPress key, preventDefault key )
            )


preventDefault key =
    key == Up || key == Down


type KeyPressed
    = Up
    | Down
    | Escape
    | Enter
    | Space
    | Other


toKeyPressed : String -> KeyPressed
toKeyPressed key =
    case key of
        "ArrowUp" ->
            Up

        "ArrowDown" ->
            Down

        "Escape" ->
            Escape

        "Enter" ->
            Enter

        " " ->
            Space

        _ ->
            Other



-- HELEPRS


findPrev : String -> List String -> Maybe String
findPrev selectedId ids =
    List.foldr (getAdjacent selectedId) Nothing ids


findNext : String -> List String -> Maybe String
findNext selectedId ids =
    List.foldl (getAdjacent selectedId) Nothing ids


getAdjacent : String -> String -> Maybe String -> Maybe String
getAdjacent selectedId currentId resultId =
    case resultId of
        Nothing ->
            if currentId == selectedId then
                Just selectedId

            else
                Nothing

        Just id ->
            if id == selectedId then
                Just currentId

            else
                Just id
