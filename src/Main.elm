module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, placeholder, type_, name, value)
import Html.Events exposing (onInput, onClick)

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model = 
  { todos: List Todo
  , input: String 
  }

type alias Todo =
  { id: ID
  , text: String
  , done: Bool
  }

newTodo : List Todo -> String -> Todo
newTodo todos text =
  let
    biggestID = List.foldl min 0 <| List.map .id <| todos
  in
    Todo biggestID text False

type alias ID = Int

init : () -> (Model, Cmd Msg)
init _ =
  ( Model 
      [ Todo 0 "Experiment with FCIS" False
      , Todo 1 "Create a full webapp with Elm" False
      , Todo 2 "Finish reading Sapiens book" False
      ]
      ""
  , Cmd.none)

type Msg 
  = NoOp
  | InputTodo String
  | AddTodo
  | DeleteTodo ID
  | DoneTodo ID

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)

    InputTodo value ->
      ( { model | input = value }, Cmd.none )

    AddTodo ->
      let
        newTodos = (newTodo model.todos model.input) :: model.todos
      in
        ( { todos = newTodos, input = "" }, Cmd.none )

    DeleteTodo id ->
      let
        newTodos = List.filter (.id >> (/=) id) model.todos
      in
        ( { todos = newTodos, input = "" }, Cmd.none )

    DoneTodo id ->
      let
        newTodos = List.map 
          (\todo -> if todo.id == id then { todo | done = True } else todo) 
          model.todos
      in
        ( { todos = newTodos, input = "" }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Model -> Browser.Document Msg
view model =
  { title = "Todo List"
  , body = 
    [ div [ class "container" ] 
      [ h1 [] [ text "Todo List!" ]
      , div [ class "form" ] 
        [ input 
          [ placeholder "Add something to do..."
          , type_ "text"
          , name "input" 
          , value model.input
          , onInput InputTodo ] [] 
        , button [ onClick AddTodo ] [ text "Add" ] 
        ]
      , ul [] (List.map viewTodo model.todos)
      ]
    ]
  }

viewTodo : Todo -> Html Msg
viewTodo todo =
  if todo.done then
    li [ class "done" ] 
      [ text todo.text
      , div [ class "actions" ]
        [ viewDeleteTodo todo ]
      ]
  else
    li [] 
      [ text todo.text
      , div [ class "actions" ] 
        [ button [ class "done", onClick (DoneTodo todo.id) ] [ text "✔" ]
        , viewDeleteTodo todo
        ]
      ]

viewDeleteTodo : Todo -> Html Msg
viewDeleteTodo todo =
  button [ class "delete", onClick (DeleteTodo todo.id) ] [ text "✖" ]
