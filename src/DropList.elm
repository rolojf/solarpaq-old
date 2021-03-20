module DropList exposing (..)

import Browser.Dom as Dom
import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class)
import Html.Attributes.Aria as Aria
import Html.Events
import Json.Decode as Decode
import Shared
import Svg
import Svg.Attributes
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


view : Model -> Html Msg
view model =
    div [ class "mt-4 w-96" ]
        [ Html.label
            [ Attr.id "listbox-label"
            , class "block text-sm font-medium text-gray-700"
            ]
            [ Html.text "Asignado a:" ]
        , div
            [ class "mt-1 relative"
            , Attr.id dropdownElementId
            , Html.Events.preventDefaultOn "keydown" keyDecoder
            , Html.Events.on "focusout" (onFocusOut dropdownElementId)
            ]
            [ Html.button
                [ class "relative w-full bg-white border border-gray-300 rounded-md shadow-sm pl-3 pr-10 py-2 text-left cursor-default focus:outline-none focus:ring-1 focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm"
                , Aria.ariaHasPopup "listbox"
                , Aria.ariaExpanded <|
                    if model.open then
                        "true"

                    else
                        "false"
                , Aria.ariaLabelledby "listbox-label"
                , Html.Events.onClick Toggle
                ]
                [ div
                    [ class "flex items-center" ]
                    [ Html.span
                        [ Aria.ariaLabel "Online"
                        , class "bg-green-400 flex-shrink-0 inline-block h-2 w-2 rounded-full"
                        ]
                        []
                    , Html.span
                        [ class <|
                            "ml-3 block truncate"
                                ++ (case model.selectedId of
                                        Just _ ->
                                            " font-bold"

                                        Nothing ->
                                            ""
                                   )
                        ]
                        [ Html.text (getButtonText model "Select...") ]
                    ]
                ]
            , if model.open then
                viewList model

              else
                Html.span
                    [ class "absolute inset-y-0 right-0 flex items-center pr-2 pointer-events-none"
                    , Aria.ariaHidden True
                    ]
                    [ heroIconSolidSelector ]
            ]
        ]


heroIconSolidSelector =
    Svg.svg
        [ Svg.Attributes.class "h-5 w-5 text-gray-400"
        , Svg.Attributes.viewBox "0 0 20 20"
        , Svg.Attributes.fill "currentColor"
        ]
        [ Svg.path
            [ Svg.Attributes.fillRule "evenodd"
            , Svg.Attributes.d "M10 3a1 1 0 01.707.293l3 3a1 1 0 01-1.414 1.414L10 5.414 7.707 7.707a1 1 0 01-1.414-1.414l3-3A1 1 0 0110 3zm-3.707 9.293a1 1 0 011.414 0L10 14.586l2.293-2.293a1 1 0 011.414 1.414l-3 3a1 1 0 01-1.414 0l-3-3a1 1 0 010-1.414z"
            , Svg.Attributes.clipRule "evenodd"
            ]
            []
        ]


heroIconSolidCheckMark =
    Svg.svg
        [ Svg.Attributes.class "h-5 w-5"
        , Svg.Attributes.viewBox "0 0 20 20"
        , Svg.Attributes.fill "currentColor"
        ]
        [ Svg.path
            [ Svg.Attributes.fillRule "evenodd"
            , Svg.Attributes.d "M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z"
            , Svg.Attributes.clipRule "evenodd"
            ]
            []
        ]


viewList : Model -> Html Msg
viewList model =
    div
        [ class "absolute mt-1 w-full rounded-md bg-white shadow-lg" ]
        [ Html.ul
            [ class "max-h-60 rounded-md py-1 text-base ring-1 ring-black ring-opacity-5 overflow-auto focus:outline-none sm:text-sm"
            , Attr.attribute "role" "listbox"
            , Aria.ariaLabelledby "listbox-label"
            , Aria.ariaActiveDescendant "listbox-item-3"
            , Attr.tabindex -1
            ]
            (model.options
                |> List.map (viewOption model)
            )
        ]


viewOption : Model -> Option -> Html Msg
viewOption model option =
    let
        isSelected =
            maybeEqual model.selectedId option.id
    in
    Html.li
        [ Attr.attribute "role" "option"
        , Attr.id option.id
        , Attr.tabindex -1
        , Html.Events.onClick (SelectOption option.id)
        , class "text-gray-900 cursor-default select-none relative py-2 pl-3 pr-9"
        , Aria.ariaSelected <|
            if isSelected then
                "true"

            else
                "false"
        ]
        [ div
            [ class "flex items-center" ]
            [ Html.span
                [ class "bg-green-400 flex-shrink-0 inline-block h-2 w-2 rounded-full"
                , Aria.ariaLabel "Online"
                ]
                []
            , Html.span
                [ class <|
                    "ml-3 block truncate "
                        ++ (if isSelected then
                                "font-semibold"

                            else if maybeEqual model.focusedId option.id then
                                "text-white font-normal bg-indigo-600"

                            else
                                "text-gray-900 text-normal"
                           )
                ]
                [ Html.text option.label
                , Html.span
                    [ class "sr-only" ]
                    [ Html.text "is Online" ]
                ]
            ]
        , if isSelected then
            Html.span
                [ class "absolute inset-y-0 right-0 flex items-center pr-4"
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
