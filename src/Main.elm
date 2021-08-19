module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing 
  (class, placeholder, type_, name, value, disabled)
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
  , mode: Mode
  }

type Mode
  = View
  | Add String
  | Edit EditPayload

type alias EditPayload =
  { todo: Todo
  , input: String
  }

isEditMode : Model -> Bool
isEditMode model =
  case model.mode of
    Edit _ -> True
    _ -> False

getEdit : (EditPayload -> a) -> a -> Mode -> a
getEdit f default mode =
  case mode of
    Edit payload ->
      f payload
    _ ->
      default

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
      (Add "")
  , Cmd.none
  )

type Msg 
  = NoOp
  | DeleteTodo ID
  | DoneTodo ID
  | RequestEdit Todo
  | InputEditTodo String
  | FinishEdit
  | BackToView
  | RequestAdd
  | InputAddTodo String
  | FinishAdd

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)

    FinishAdd ->
      let
        newTodos = case model.mode of
          Add value -> (newTodo model.todos value) :: model.todos
          _ -> model.todos
      in
        ( { model | todos = newTodos, mode = View }, Cmd.none )

    DeleteTodo id ->
      let
        newTodos = List.filter (.id >> (/=) id) model.todos
      in
        ( { model | todos = newTodos }, Cmd.none )

    DoneTodo id ->
      let
        newTodos = List.map 
          (\ todo -> if todo.id == id then { todo | done = True } else todo) 
          model.todos
      in
        ( { model | todos = newTodos }, Cmd.none )

    RequestEdit todo ->
      ( { model | mode = Edit (EditPayload todo todo.text) }, Cmd.none )

    InputEditTodo value ->
      case model.mode of
        Edit payload ->
          ( { model | mode = Edit { payload | input = value } }, Cmd.none )
        _ ->
          ( model, Cmd.none )

    FinishEdit ->
      let
        editID = getEdit (.todo >> .id) -1 model.mode 
        editValue = getEdit .input "" model.mode 
        newTodos = List.map 
          (\ todo -> 
            if todo.id == editID then 
              { todo | text = editValue } 
            else 
              todo 
          ) model.todos
      in
        ( { model | todos = newTodos, mode = View }, Cmd.none )

    BackToView ->
      ( { model | mode = View }, Cmd.none )

    RequestAdd ->
      ( { model | mode = Add "" }, Cmd.none )

    InputAddTodo value ->
      case model.mode of
        Add _ ->
          ( { model | mode = Add value }, Cmd.none )
        _ ->
          ( model, Cmd.none )

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
          , disabled (isEditMode model)
          ] [] 
        , button [ disabled (isEditMode model) ] [ text "Add" ] 
        ]
      , ul [] 
        ( li [ class "add" ] 
            [ case model.mode of 
              Add value ->
                div [ class "add-form" ]
                  [ viewTodoInput InputAddTodo value
                  , div [] 
                      [ button [ onClick FinishAdd, class "save" ] 
                          [ text "Save" ]
                      , button [ onClick BackToView, class "cancel" ] 
                          [ text "Cancel" ]
                      ]
                  ]
              _ ->
                button 
                  [ class "add-btn"
                  , onClick RequestAdd ] 
                  [ text "+ Add Todo" ]
            ]
          ::
          ( List.map (\ todo -> 
            case model.mode of
              Edit payload ->
                if todo == payload.todo then
                  viewEditTodo payload
                else
                  viewTodo model todo 
              _ -> 
                viewTodo model todo 
            ) 
            model.todos
          ) 
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
    [ viewTodoInput InputEditTodo payload.input
    , div [ class "actions" ]
      [ button [ class "done", onClick FinishEdit ] [ text "✔" ]
      , button [ class "delete", onClick BackToView ] [ text "✖" ] 
      ]
    ]

viewTodoInput : (String -> Msg) -> String -> Html Msg
viewTodoInput onInputFunc val =
  input 
    [ class "todo-input"
    , placeholder "Write something to do..."
    , type_ "text"
    , name "input" 
    , value val
    , onInput onInputFunc
    ] []

viewDeleteTodo : Todo -> Html Msg
viewDeleteTodo todo =
  button [ class "delete", onClick (DeleteTodo todo.id) ] [ text "✖" ]
