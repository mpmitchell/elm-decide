port module Decide exposing (..)

import Dom
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Json.Decode as Json
import Random
import String
import Task


port setStorage : Model -> Cmd msg


main : Program (Maybe Model)
main =
    App.programWithFlags
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { entries : List Entry
    , selectedEntry : Maybe Entry
    , selectedId : Int
    , field : String
    , uid : Int
    }


type alias Entry =
    { description : String
    , editing : Bool
    , id : Int
    }


emptyModel : Model
emptyModel =
    { entries = []
    , selectedEntry = Nothing
    , selectedId = -1
    , field = ""
    , uid = 0
    }


emptyEntry : Entry
emptyEntry =
    { description = ""
    , editing = False
    , id = 0
    }


newEntry : String -> Int -> Entry
newEntry description id =
    { description = description
    , editing = False
    , id = id
    }


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
    Maybe.withDefault emptyModel savedModel ! []



-- UPDATE


type Msg
    = NoOp
    | UpdateField String
    | AddEntry
    | ToggleEditing Int Bool
    | EditEntry Int String
    | ToggleEditingSelected Bool
    | EditSelectedEntry String
    | RemoveEntry Int
    | RemoveAllEntries
    | SelectRandomEntry
    | NewRandom Int


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch
            [ setStorage
                { newModel
                    | entries =
                        List.map
                            (\e -> { e | editing = False })
                            newModel.entries
                    , selectedEntry = Nothing
                    , selectedId = -1
                }
            , cmds
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateField field ->
            { model | field = field } ! []

        AddEntry ->
            if not <| String.isEmpty model.field then
                { model
                    | entries = List.append model.entries (newEntry model.field model.uid :: [])
                    , field = ""
                    , uid = model.uid + 1
                }
                    ! []
            else
                model ! []

        ToggleEditing id isEditing ->
            let
                isNotEmpty =
                    case
                        List.head <|
                            List.filter (\e -> e.id == id) model.entries
                    of
                        Nothing ->
                            False

                        Just entry ->
                            not <| String.isEmpty entry.description

                updateEntry e =
                    if e.id == id then
                        { e | editing = isEditing }
                    else
                        e

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
                    { model | entries = List.map updateEntry model.entries }
                        ! [ focus ]
                else
                    removeEntry model id ! []

        EditEntry id description ->
            updateEntry model id description ! []

        ToggleEditingSelected isEditing ->
            let
                entry =
                    List.head <| List.filter (\e -> e.id == model.selectedId) model.entries

                focus =
                    let
                        task =
                            if isEditing == True then
                                Dom.focus "selected"
                            else
                                Task.succeed ()
                    in
                        Task.perform (\_ -> NoOp) (\_ -> NoOp) task
            in
                case ( entry, model.selectedEntry ) of
                    ( Just entry, Just selectedEntry ) ->
                        if not <| String.isEmpty selectedEntry.description then
                            { model
                                | selectedEntry = Just { selectedEntry | editing = isEditing }
                            }
                                ! [ focus ]
                        else
                            removeEntry model selectedEntry.id ! []

                    _ ->
                        model ! []

        EditSelectedEntry description ->
            case model.selectedEntry of
                Nothing ->
                    model ! []

                Just entry ->
                    updateEntry model entry.id description ! []

        RemoveEntry id ->
            removeEntry model id ! []

        RemoveAllEntries ->
            { model
                | entries = []
                , selectedEntry = Nothing
                , selectedId = -1
                , uid = 0
            }
                ! []

        SelectRandomEntry ->
            ( model
            , Random.generate NewRandom (Random.int 0 (List.length model.entries - 1))
            )

        NewRandom int ->
            let
                selectedEntry =
                    List.head <| List.drop int model.entries

                selectedId =
                    case selectedEntry of
                        Nothing ->
                            -1

                        Just entry ->
                            entry.id
            in
                { model
                    | selectedEntry = selectedEntry
                    , selectedId = selectedId
                }
                    ! []

        NoOp ->
            model ! []


removeEntry : Model -> Int -> Model
removeEntry model id =
    let
        ( selectedEntry, selectedId ) =
            case model.selectedEntry of
                Nothing ->
                    ( Nothing, -1 )

                Just entry ->
                    if entry.id == id then
                        ( Nothing, -1 )
                    else
                        ( Just entry, entry.id )

        model =
            { model
                | entries = List.filter (\e -> e.id /= id) model.entries
                , selectedEntry = selectedEntry
                , selectedId = selectedId
            }
    in
        if List.isEmpty model.entries then
            { model | uid = 0 }
        else
            model


updateEntry : Model -> Int -> String -> Model
updateEntry model id description =
    let
        entry =
            List.head <| List.filter (\e -> e.id == id) model.entries
    in
        { model
            | entries =
                List.map
                    (\e ->
                        if e.id == id then
                            { e | description = description }
                        else
                            e
                    )
                    model.entries
            , selectedEntry =
                case ( entry, model.selectedEntry ) of
                    ( Just entry, Just selectedEntry ) ->
                        if entry.id == selectedEntry.id then
                            Just { selectedEntry | description = description }
                        else
                            Just selectedEntry

                    _ ->
                        model.selectedEntry
        }



-- VIEW


view : Model -> Html Msg
view model =
    article []
        [ lazy viewHeader model.field
        , lazy viewEntries model.entries
        , lazy viewSelectedEntry model.selectedEntry
        , viewFooter
        ]


viewHeader : String -> Html Msg
viewHeader field =
    header []
        [ input
            [ onInput UpdateField
            , onEnter AddEntry
            , autofocus True
            , placeholder "Enter text"
            , value field
            ]
            []
        , button
            [ onClick AddEntry ]
            [ text "Add" ]
        ]


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
                , value entry.description
                ]
                []
            ]
    else
        li
            [ onClick (ToggleEditing entry.id True)
            ]
            [ span [] [ text entry.description ]
            , button [ onClick (RemoveEntry entry.id) ] [ text "X" ]
            ]


viewSelectedEntry : Maybe Entry -> Html Msg
viewSelectedEntry selectedEntry =
    let
        ( entry, display, innerText ) =
            case selectedEntry of
                Nothing ->
                    ( emptyEntry, "none", text "" )

                Just entry ->
                    ( entry, "block", text entry.description )
    in
        section
            [ style [ ( "display", display ) ] ]
            [ if entry.editing then
                input
                    [ id "selected"
                    , onInput EditSelectedEntry
                    , onEnter (ToggleEditingSelected False)
                    , onBlur (ToggleEditingSelected False)
                    , value entry.description
                    ]
                    []
              else
                h1
                    [ id "selected"
                    , onClick (ToggleEditingSelected True)
                    ]
                    [ innerText ]
            ]


viewFooter : Html Msg
viewFooter =
    footer []
        [ button [ onClick RemoveAllEntries ] [ text "Clear" ]
        , button [ onClick SelectRandomEntry ] [ text "Pick" ]
        ]
