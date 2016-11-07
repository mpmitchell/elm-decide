port module PickerList exposing (..)

import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Json.Decode as Json
import Random
import String
import Task


-- MODEL


type alias PickerList =
    { name : String
    , id : Int
    , editing : Bool
    , entries : List Entry
    , selectedId : Int
    , uid : Int
    }


type alias Entry =
    { text : String
    , editing : Bool
    , id : Int
    }


newList : String -> Int -> Bool -> PickerList
newList name id editing  =
    { name = name
    , id = id
    , editing = editing
    , entries = []
    , selectedId = -1
    , uid = 0
    }


newEntry : String -> Int -> Entry
newEntry text id =
    { text = text
    , editing = False
    , id = id
    }


selectedEntry : PickerList -> Maybe Entry
selectedEntry list =
    List.head <| List.filter (\e -> e.id == list.selectedId) list.entries



-- UPDATE


type Msg
    = NoOp
    | AddEntry String
    | ToggleEditing Int Bool
    | EditEntry Int String
    | RemoveEntry Int
    | RemoveAllEntries
    | SelectRandomEntry
    | NewRandom Int


update : Msg -> PickerList -> ( PickerList, Cmd Msg )
update msg list =
    case msg of
        AddEntry text ->
            if not <| String.isEmpty text then
                { list
                    | entries = List.append list.entries (newEntry text list.uid :: [])
                    , uid = list.uid + 1
                }
                    ! []
            else
                list ! []

        ToggleEditing id isEditing ->
            let
                isNotEmpty =
                    case
                        List.head <| List.filter (\e -> e.id == id) list.entries
                    of
                        Nothing ->
                            False

                        Just entry ->
                            not <| String.isEmpty entry.text

                updateEntry entry =
                    if entry.id == id then
                        { entry | editing = isEditing }
                    else
                        entry

                focus =
                    let
                        task =
                            if isEditing == True then
                                Dom.focus (toString id)
                            else
                                Task.succeed ()
                    in
                        Task.perform (\_ -> NoOp) (\_ -> NoOp) task
            in
                if isNotEmpty then
                    { list | entries = List.map updateEntry list.entries }
                        ! [ focus ]
                else
                    removeEntry list id ! []

        EditEntry id text ->
            let
                entry =
                    List.head <| List.filter (\e -> e.id == id) list.entries

                updateEntry e =
                    if e.id == id then
                        { e | text = text }
                    else
                        e
            in
                { list | entries = List.map updateEntry list.entries }
                    ! []

        RemoveEntry id ->
            removeEntry list id ! []

        RemoveAllEntries ->
            { list
                | entries = []
                , selectedId = -1
                , uid = 0
            }
                ! []

        SelectRandomEntry ->
            list
                ! [ Random.generate
                        NewRandom
                        (Random.int 0 (List.length list.entries - 1))
                  ]

        NewRandom int ->
            let
                selectedEntry =
                    List.head <| List.drop int list.entries

                selectedId =
                    case selectedEntry of
                        Nothing ->
                            -1

                        Just entry ->
                            entry.id
            in
                { list | selectedId = selectedId }
                    ! []

        NoOp ->
            list ! []


removeEntry : PickerList -> Int -> PickerList
removeEntry list id =
    let
        list =
            { list
                | entries = List.filter (\e -> e.id /= id) list.entries
                , selectedId =
                    if id == list.selectedId then
                        -1
                    else
                        list.selectedId
            }
    in
        if List.isEmpty list.entries then
            { list | uid = 0 }
        else
            list



-- VIEW


view : PickerList -> Html Msg
view list =
    main' []
        [ lazy viewEntries list.entries
        , lazy viewSelectedEntry list
        ]


viewEntries : List Entry -> Html Msg
viewEntries entries =
    ul [] (List.map viewEntry entries)


viewEntry : Entry -> Html Msg
viewEntry entry =
    if entry.editing then
        li []
            [ input
                [ id (toString entry.id)
                , onInput (EditEntry entry.id)
                , onEnter (ToggleEditing entry.id False)
                , onBlur (ToggleEditing entry.id False)
                , value entry.text
                ]
                []
            ]
    else
        li
            []
            [ span [] [ text entry.text ]
            , button
                [ class "edit"
                , onClick (ToggleEditing entry.id True)
                ]
                [ i [ class "material-icons" ] [ text "edit" ] ]
            , button
                [ onClick (RemoveEntry entry.id) ]
                [ i [ class "material-icons" ] [ text "remove" ] ]
            ]


viewSelectedEntry : PickerList -> Html Msg
viewSelectedEntry list =
    case selectedEntry list of
        Nothing ->
            text ""

        Just entry ->
            if String.isEmpty entry.text then
                text ""
            else
                section []
                    [ h1 [ id "selected" ] [ text entry.text ] ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        tagger code =
            if code == 13 then
                msg
            else
                NoOp
    in
        on "keydown" (Json.map tagger keyCode)
