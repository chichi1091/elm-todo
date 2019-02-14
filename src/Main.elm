module Main exposing (Model, Msg(..), init, main, model, showList, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


type alias Model =
    { newTodo : String
    , todoList : List String
    }


model : Model
model =
    { newTodo = ""
    , todoList = []
    }


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = Change String
    | Add
    | Delete Int


update : Msg -> Model -> Model
update msg m =
    let
        isSpace =
            String.trim >> String.isEmpty
    in
    case msg of
        Change str ->
            { model | newTodo = str }

        Add ->
            if isSpace model.newTodo then
                model

            else
                { model
                    | todoList = model.newTodo :: model.todoList
                    , newTodo = ""
                }

        Delete n ->
            let
                t =
                    model.todoList
            in
            { model
                | todoList = List.take n t ++ List.drop (n + 1) t
            }



---- VIEW ----


view : Model -> Html Msg
view m =
    div []
        [ input
            [ type_ "text"
            , placeholder "input your todo"
            , onInput Change
            , value model.newTodo
            ]
            []
        , button [ onClick Add ] [ text "add todo" ]
        , div [] (showList model.todoList)
        ]


showList : List String -> List (Html Msg)
showList =
    let
        todos =
            List.indexedMap (,)

        column ( n, s ) =
            div []
                [ text s
                , button [ onClick (Delete n) ] [ text "×" ]
                ]
    in
    todos >> List.map column



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
