port module Main exposing (..)

import Dom
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Json.Decode as Json
import PickerList exposing (PickerList, Entry)
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
    { lists : List PickerList
    , selectedId : Int
    , navbarOpen : Bool
    , field : String
    , uid : Int
    }


emptyModel : Model
emptyModel =
    { lists = [ PickerList.newList "List" 0 False ]
    , selectedId = 0
    , navbarOpen = False
    , field = ""
    , uid = 1
    }


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
    Maybe.withDefault emptyModel savedModel ! []


selectedList : Model -> Maybe PickerList
selectedList model =
    List.head <| List.filter (\l -> l.id == model.selectedId) model.lists



-- UPDATE


type Msg
    = NoOp
    | UpdateField String
    | UpdateList PickerList.Msg
    | AddEntry String
    | ToggleNav
    | NewList
    | SelectList Int
    | ToggleEditing Int Bool
    | EditList Int String
    | RemoveList Int


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( model, cmds ) =
            update msg model

        cleanLists list =
            { list
                | editing = False
                , entries =
                    List.map (\e -> { e | editing = False }) list.entries
                , selectedId = -1
            }
    in
        ( model
        , Cmd.batch
            [ setStorage
                { model
                    | lists = List.map cleanLists model.lists
                    , navbarOpen = False
                }
            , cmds
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateField field ->
            { model | field = field } ! []

        UpdateList msg ->
            updateList msg model

        AddEntry text ->
            let
                ( model, msg ) =
                    updateList (PickerList.AddEntry text) model
            in
                { model | field = "" }
                    ! [ msg ]

        ToggleNav ->
            { model | navbarOpen = not model.navbarOpen }
                ! []

        NewList ->
            let
                focus =
                    Task.perform
                        (\_ -> NoOp)
                        (\_ -> NoOp)
                        (Dom.focus (toString model.uid))
            in
                { model
                    | lists = (PickerList.newList "List" model.uid True) :: model.lists
                    , uid = model.uid + 1
                }
                    ! [ focus ]

        SelectList id ->
            { model | selectedId = id }
                ! []

        ToggleEditing id isEditing ->
            let
                isNotEmpty =
                    case
                        List.head <| List.filter (\l -> l.id == id) model.lists
                    of
                        Nothing ->
                            False

                        Just list ->
                            not <| String.isEmpty list.name

                updateList list =
                    if list.id == id then
                        if isNotEmpty then
                            { list | editing = isEditing }
                        else
                            { list
                                | name = "List"
                                , editing = isEditing
                            }
                    else
                        list

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
                { model | lists = List.map updateList model.lists }
                    ! [ focus ]

        EditList id name ->
            let
                updateList list =
                    if list.id == id then
                        { list | name = name }
                    else
                        list
            in
                { model | lists = List.map updateList model.lists }
                    ! []

        RemoveList id ->
            removeList model id ! []

        NoOp ->
            model ! []


updateList : PickerList.Msg -> Model -> ( Model, Cmd Msg )
updateList msg model =
    case selectedList model of
        Nothing ->
            model ! []

        Just list ->
            let
                ( newList, listMsg ) =
                    PickerList.update msg list

                updateList list =
                    if list.id == newList.id then
                        newList
                    else
                        list
            in
                { model | lists = List.map updateList model.lists }
                    ! [ Cmd.map (\m -> UpdateList m) listMsg ]


removeList : Model -> Int -> Model
removeList model id =
    let
        model =
            { model
                | lists = List.filter (\l -> l.id /= id) model.lists
                , selectedId =
                    if id == model.selectedId then
                        -1
                    else
                        model.selectedId
            }
    in
        if List.isEmpty model.lists then
            { model
                | lists = [ PickerList.newList "List" 0 False ]
                , selectedId = 0
                , uid = 1
            }
        else
            model



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "container" ]
        [ lazy viewHeader model.field
        , lazy viewNavbar model
        , lazy viewList model
        , viewFooter
        ]


viewHeader : String -> Html Msg
viewHeader field =
    header []
        [ button
            [ class "menu"
            , onClick ToggleNav
            ]
            [ i [ class "material-icons" ] [ text "menu" ] ]
        , input
            [ onInput UpdateField
            , onEnter (AddEntry field)
            , autofocus True
            , placeholder "Enter text"
            , value field
            ]
            []
        , button
            [ onClick (AddEntry field) ]
            [ i [ class "material-icons" ] [ text "add" ] ]
        ]


viewNavbar : Model -> Html Msg
viewNavbar model =
    let
        viewList list =
            if list.editing then
                li []
                    [ input
                        [ id (toString list.id)
                        , onInput (EditList list.id)
                        , onEnter (ToggleEditing list.id False)
                        , onBlur (ToggleEditing list.id False)
                        , value list.name
                        ]
                        []
                    ]
            else
                li
                    [ if list.id == model.selectedId then
                        class "selected"
                      else
                        class ""
                    ]
                    [ span
                        [ onClick (SelectList list.id) ]
                        [ text list.name ]
                    , button
                        [ class "edit"
                        , onClick (ToggleEditing list.id True)
                        ]
                        [ i [ class "material-icons" ] [ text "edit" ] ]
                    , button
                        [ onClick (RemoveList list.id) ]
                        [ i [ class "material-icons" ] [ text "remove" ] ]
                    ]
    in
        if model.navbarOpen == True then
            nav [ class "open" ]
                [ div []
                    [ button
                        [ class "menu"
                        , onClick ToggleNav
                        ]
                        [ i [ class "material-icons" ] [ text "menu" ] ]
                    , button [ onClick NewList ]
                        [ i [ class "material-icons" ] [ text "add" ] ]
                    ]
                , ul [] (List.map viewList model.lists)
                ]
        else
            nav [ class "closed" ]
                [ button [ onClick NewList ] [ text "New List" ]
                , ul [] (List.map viewList model.lists)
                ]


viewList : Model -> Html Msg
viewList model =
    case selectedList model of
        Nothing ->
            main' [] []

        Just list ->
            App.map (\m -> UpdateList m) (PickerList.view list)


viewFooter : Html Msg
viewFooter =
    footer []
        [ button [ onClick (UpdateList PickerList.RemoveAllEntries) ] [ text "Clear" ]
        , button [ onClick (UpdateList PickerList.SelectRandomEntry) ] [ text "Pick" ]
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
