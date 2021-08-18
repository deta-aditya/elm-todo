module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html.Events exposing (onClick)

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Msg 
  = NoOp
  | InputTodo String
  | AddTodo

type alias Model = 
  { todos: List String
  , input: String }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model 
      [ "Experiment with FCIS"
      , "Create a full webapp with Elm"
      , "Finish reading Sapiens book"
      ]
      ""
  , Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)

    InputTodo value ->
      ( { model | input = value }, Cmd.none )

    AddTodo ->
      ( Model (model.input :: model.todos) "", Cmd.none )

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

viewTodo : String -> Html Msg
viewTodo txt =
  li [] 
    [ text txt
    , div [ class "actions" ] 
      [ button [ class "done" ] [ text "✔" ] 
      , button [ class "delete" ] [ text "✖" ]
      ]
    ]