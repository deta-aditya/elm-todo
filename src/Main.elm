module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, placeholder, type_, name, value)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (disabled)

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
  , edit: Maybe EditPayload
  }

type alias EditPayload =
  { todo: Todo
  , input: String
  }

type alias Todo =
  { id: ID
  , text: String
  , done: Bool
  }
type alias ID = Int

newTodo : List Todo -> String -> Todo
newTodo todos text =
  let
    biggestID = List.foldl max 0 <| List.map .id <| todos
  in
    Todo (biggestID + 1) text False

init : () -> (Model, Cmd Msg)
init _ =
  ( Model 
      [ Todo 0 "Experiment with FCIS" False
      , Todo 1 "Create a full webapp with Elm" False
      , Todo 2 "Finish reading Sapiens book" False
      ]
      ""
      Nothing
  , Cmd.none)

type Msg 
  = NoOp
  | InputTodo String
  | AddTodo
  | DeleteTodo ID
  | DoneTodo ID
  | RequestEdit Todo
  | InputEditTodo String
  | FinishEdit
  | CancelEdit

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)

    InputTodo value ->
      ( { model | input = value }, Cmd.none )

    AddTodo ->
      let
        newTodos = case model.input of 
          "" -> model.todos
          _ -> (newTodo model.todos model.input) :: model.todos
      in
        ( { model | todos = newTodos, input = "" }, Cmd.none )

    DeleteTodo id ->
      let
        newTodos = List.filter (.id >> (/=) id) model.todos
      in
        ( { model | todos = newTodos, input = "" }, Cmd.none )

    DoneTodo id ->
      let
        newTodos = List.map 
          (\todo -> if todo.id == id then { todo | done = True } else todo) 
          model.todos
      in
        ( { model | todos = newTodos, input = "" }, Cmd.none )

    RequestEdit todo ->
      ( { model | edit = Just (EditPayload todo todo.text) }, Cmd.none )

    InputEditTodo value ->
      let
        newEdit = Maybe.map (\old -> { old | input = value } ) model.edit
      in
        ( { model | edit = newEdit }, Cmd.none )

    FinishEdit ->
      let
        editID = (Maybe.map (.todo >> .id) >> Maybe.withDefault -1) model.edit
        editValue = (Maybe.map .input >> Maybe.withDefault "") model.edit
        newTodos = List.map 
          (\todo -> 
            if todo.id == editID then 
              { todo | text = editValue } 
            else 
              todo 
          ) model.todos
      in
        ( { model | todos = newTodos, edit = Nothing }, Cmd.none )

    CancelEdit ->
      ( { model | edit = Nothing }, Cmd.none )

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
          , disabled (isEditMode model)
          , onInput InputTodo ] [] 
        , button [ onClick AddTodo, disabled (isEditMode model) ] [ text "Add" ] 
        ]
      , ul [] 
        (List.map ( \todo -> 
          case model.edit of
            Just payload ->
              if todo == payload.todo then
                viewEditTodo payload
              else
                viewTodo model todo 
            Nothing -> 
              viewTodo model todo 
          ) 
          model.todos
        )
      ]
    ]
  }

viewTodo : Model -> Todo -> Html Msg
viewTodo model todo =
  if todo.done then
    li [ class "done" ] 
      [ text todo.text
      , div [ class "actions" ]
        [ viewDeleteTodo todo ]
      ]
  else
    li [] 
      [ text todo.text
      , viewRegularActions model todo
      ]

viewRegularActions : Model -> Todo -> Html Msg
viewRegularActions model todo =
  div [ class "actions" ]
    ( if isEditMode model then
        []
      else
        [ button [ class "edit", onClick (RequestEdit todo) ] [ text "✎" ]
        , button [ class "done", onClick (DoneTodo todo.id) ] [ text "✔" ]
        , viewDeleteTodo todo
        ]
    )

viewEditTodo : EditPayload -> Html Msg
viewEditTodo payload =
  li [ class "editable" ]
    [ input 
      [ placeholder "Add something to do..."
      , type_ "text"
      , name "input" 
      , value payload.input
      , onInput InputEditTodo 
      ] [] 
    , div [ class "actions" ]
      [ button [ class "done", onClick FinishEdit ] [ text "✔" ]
      , button [ class "delete", onClick CancelEdit ] [ text "✖" ] 
      ]
    ]

viewDeleteTodo : Todo -> Html Msg
viewDeleteTodo todo =
  button [ class "delete", onClick (DeleteTodo todo.id) ] [ text "✖" ]

isEditMode : Model -> Bool
isEditMode model =
  case model.edit of
    Just _ -> True
    Nothing -> False
