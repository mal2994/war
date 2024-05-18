module ExampleRndNum exposing (..)

import Browser
import Html exposing (Html, div, text)
import Random


type alias Model =
    Int


initialModel : Int
initialModel =
    -1


initialCmd : Cmd Msg
initialCmd =
    Random.generate GotRandomInt (Random.int 0 100)


type Msg
    = GotRandomInt Int


init : a -> ( Model, Cmd Msg )
init _ =
    ( initialModel, initialCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotRandomInt n ->
            ( n, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ text (String.fromInt model) ]


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }



-- An exercise for the reader! Can you add a button to generate another random number? Hint: It requires adding a variant to Msg to handle the onClick event.
