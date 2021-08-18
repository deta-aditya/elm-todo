module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (type_)
import Html.Attributes exposing (name)
import Html.Attributes exposing (class)

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

type alias Model = Int

init : () -> (Model, Cmd Msg)
init _ =
  (0, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update _ model =
  (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Model -> Browser.Document Msg
view _ =
  { title = "Todo List"
  , body = 
    [ div [ class "container" ] 
      [ h1 [] [ text "Todo List!" ]
      , div [ class "form" ] 
        [ input [ type_ "text", name "input" ] [] 
        , button [] [ text "Add" ] 
        ]
      , ul [] 
        [ viewTodo "Experiment with FCIS" 
        , viewTodo "Create a full webapp with Elm"
        ] 
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