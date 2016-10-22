port module Decide exposing (..)

import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Json.Decode as Json
import Random
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
            { model
                | entries = List.append model.entries (newEntry model.field model.uid :: [])
                , field = ""
                , uid = model.uid + 1
            }
                ! []

        ToggleEditing id isEditing ->
            let
                updateEntry e =
                    if e.id == id then
                        { e | editing = isEditing }
                    else
                        e

                focus =
                    Dom.focus (toString id)
            in
                { model | entries = List.map updateEntry model.entries }
                    ! [ Task.perform (\_ -> NoOp) (\_ -> NoOp) focus ]

        EditEntry id description ->
            { model
                | entries =
                    List.map
                        (\e ->
                            if id == e.id then
                                { e | description = description }
                            else
                                e
                        )
                        model.entries
                , selectedEntry =
                    case model.selectedEntry of
                        Nothing ->
                            Nothing

                        Just entry ->
                            if id == entry.id then
                                Just { entry | description = description }
                            else
                                Just entry
            }
                ! []

        ToggleEditingSelected isEditing ->
            case model.selectedEntry of
                Nothing ->
                    model ! []

                Just entry ->
                    { model
                        | selectedEntry = Just { entry | editing = isEditing }
                    }
                        ! [ Task.perform
                                (\_ -> NoOp)
                                (\_ -> NoOp)
                                (Dom.focus "selected")
                          ]

        EditSelectedEntry description ->
            case model.selectedEntry of
                Nothing ->
                    model ! []

                Just entry ->
                    { model
                        | entries =
                            List.map
                                (\e ->
                                    if e.id == entry.id then
                                        { e | description = description }
                                    else
                                        e
                                )
                                model.entries
                        , selectedEntry = Just { entry | description = description }
                    }
                        ! []

        RemoveEntry id ->
            { model
                | entries = List.filter (\e -> e.id /= id) model.entries
                , selectedEntry =
                    case model.selectedEntry of
                        Nothing ->
                            Nothing

                        Just entry ->
                            if id == entry.id then
                                Nothing
                            else
                                Just entry
            }
                ! []

        RemoveAllEntries ->
            { model
                | entries = []
                , selectedEntry = Nothing
            }
                ! []

        SelectRandomEntry ->
            ( model
            , Random.generate NewRandom (Random.int 0 (List.length model.entries - 1))
            )

        NewRandom int ->
            { model | selectedEntry = List.head <| List.drop int model.entries } ! []

        NoOp ->
            model ! []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "wrapper" ]
            [ lazy viewInput model.field
            , lazy viewEntries model.entries
            , div [ class "footer" ]
                [ button [ onClick RemoveAllEntries ] [ text "Clear" ]
                , button [ onClick SelectRandomEntry ] [ text "Randomise" ]
                ]
            ]
        , lazy viewSelectedEntry model.selectedEntry
        ]


viewInput : String -> Html Msg
viewInput field =
    input
        [ onInput UpdateField
        , onEnter AddEntry
        , autofocus True
        , placeholder "Enter text"
        , value field
        ]
        []


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
            [ onDoubleClick (ToggleEditing entry.id True)
            ]
            [ span [] [ text entry.description ]
            , button [ onClick (RemoveEntry entry.id) ] [ text "X" ]
            ]


viewSelectedEntry : Maybe Entry -> Html Msg
viewSelectedEntry selectedEntry =
    let
        ( entry, visibility, innerText ) =
            case selectedEntry of
                Nothing ->
                    ( emptyEntry, "hidden", text "" )

                Just entry ->
                    ( entry, "visible", text entry.description )
    in
        div
            [ class "wrapper"
            , style [ ( "visibility", visibility ) ]
            ]
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
                div
                    [ id "selected"
                    , onDoubleClick (ToggleEditingSelected True)
                    ]
                    [ innerText ]
            ]
