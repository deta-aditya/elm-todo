module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing 
  (class, placeholder, type_, name, value, disabled, id)
import Html.Events exposing (onInput, onClick)
import Browser.Dom exposing (focus)
import Todo exposing 
  (Todo, ID, appendText, deleteByID, doneByID, replaceTitleByID, done, title)
import Task
import String exposing (contains)
import String exposing (toLower)


-- MAIN

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model = 
  { todos: List Todo
  , mode: Mode
  , query: String
  }

type Mode
  = View
  | Add String
  | Edit EditPayload

type alias EditPayload =
  { id: ID
  , input: String
  }

isEditMode : Model -> Bool
isEditMode { mode } =
  case mode of
    Edit _ -> True
    _ -> False

filteredTodos : Model -> List Todo
filteredTodos { todos, query } =
  todos |> List.filter ( title >> toLower >> contains (toLower query) )


-- INIT

init : () -> (Model, Cmd Msg)
init _ =
  ( Model 
      (
        [ "Experiment with FCIS"
        , "Create a full webapp with Elm"
        , "Finish reading Sapiens book"
        ] |> List.foldl (\ t l -> appendText l t) []
      )
      View
      ""
  , Cmd.none
  )


-- UPDATE

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
  | FocusOn String
  | Search String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)

    FinishAdd ->
      case model.mode of
        Add value ->
          ( { model | todos = appendText model.todos value
            , mode = View }, Cmd.none 
          )
        _ -> 
          update NoOp model

    DeleteTodo id ->
      ( { model | todos = deleteByID model.todos id }, Cmd.none )

    DoneTodo id ->
      ( { model | todos = doneByID model.todos id }, Cmd.none )

    RequestEdit todo ->
      update 
        (FocusOn "todo-input") 
        { model | mode = Edit ( EditPayload (Todo.id todo) (Todo.title todo) ) }

    InputEditTodo value ->
      case model.mode of
        Edit payload ->
          ( { model | mode = Edit { payload | input = value } }, Cmd.none )
        _ ->
          update NoOp model

    FinishEdit ->
      case model.mode of
        Edit { input, id } ->
          let
            newTodos = replaceTitleByID model.todos input id 
          in
            ( { model | mode = View, todos = newTodos }, Cmd.none )
        _ ->
          update NoOp model

    BackToView ->
      ( { model | mode = View }, Cmd.none )

    RequestAdd ->
      update (FocusOn "todo-input") { model | mode = Add "" }

    InputAddTodo value ->
      case model.mode of
        Add _ ->
          ( { model | mode = Add value }, Cmd.none )
        _ ->
          ( model, Cmd.none )

    FocusOn id ->
      ( model, Task.attempt (always NoOp) (focus id) )

    Search query ->
      ( { model | query = query }, Cmd.none )


-- SUB

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "Todo List"
  , body = 
    [ div [ class "container" ] 
      [ h1 [] [ text "Todo List!" ]
      , div [ class "search-form" ]  
        [ input 
          [ placeholder "Search..."
          , type_ "text"
          , name "input"
          , onInput Search
          , value model.query 
          , disabled (isEditMode model)
          ] [] 
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
                  , onClick RequestAdd 
                  ] 
                  [ text "+ Add Todo" ]
            ]
          ::
          ( List.map (\ todo -> 
            case model.mode of
              Edit payload ->
                if (Todo.id todo) == payload.id then
                  viewEditTodo payload
                else
                  viewTodo model todo 
              _ -> 
                viewTodo model todo 
            ) 
            ( filteredTodos model )
          ) 
        )
      ]
    ]
  }

viewTodo : Model -> Todo -> Html Msg
viewTodo model todo =
  if (todo |> done) then
    li [ class "done" ] 
      [ text (todo |> title)
      , div [ class "actions" ]
        [ viewDeleteTodo todo ]
      ]
  else
    li [] 
      [ text (todo |> title)
      , viewRegularActions model todo
      ]

viewRegularActions : Model -> Todo -> Html Msg
viewRegularActions model todo =
  div [ class "actions" ]
    ( if isEditMode model then
        []
      else
        [ button [ class "edit", onClick (RequestEdit todo) ] [ text "✎" ]
        , button 
          [ class "done"
          , onClick (DoneTodo (todo |> Todo.id)) 
          ] 
          [ text "✔" ]
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
    [ id "todo-input"
    , class "todo-input"
    , placeholder "Write something to do..."
    , type_ "text"
    , name "input" 
    , value val
    , onInput onInputFunc
    ] []

viewDeleteTodo : Todo -> Html Msg
viewDeleteTodo todo =
  button [ class "delete", onClick (DeleteTodo (todo |> Todo.id)) ] [ text "✖" ]
