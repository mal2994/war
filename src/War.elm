module War exposing (..)

import Browser
import Html exposing (button, div, pre, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import Random.List



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


type Msg
    = ClickedGo
    | GotRandomDeck (List Card)



-- MODEL


type Suit
    = Hearts
    | Diamonds
    | Clubs
    | Spades


type alias Card =
    { rank : Int
    , suit : Suit
    }


type alias Player =
    { hand : List Card
    , score : Int
    }


type alias Model =
    { players : ( Player, Player )
    }



-- init : Model


createDeck : List Card
createDeck =
    List.map
        (\rank ->
            List.map
                (\suit ->
                    { rank = rank, suit = suit }
                )
                [ Hearts, Diamonds, Clubs, Spades ]
        )
        (List.range 1 13)
        |> List.concat


init : a -> ( Model, Cmd Msg )
init _ =
    ( initialModel, initialCmd )


initialModel : Model
initialModel =
    { players =
        ( { hand = [], score = 0 }
        , { hand = [], score = 0 }
        )
    }


initialCmd : Cmd Msg
initialCmd =
    Random.generate GotRandomDeck (Random.List.shuffle createDeck)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedGo ->
            ( model, Cmd.none )

        GotRandomDeck d ->
            let
                ( player1, player2 ) =
                    model.players
            in
            ( { model
                | players =
                    ( { player1 | hand = List.take 26 d }
                    , { player2 | hand = List.drop 26 d }
                    )
              }
            , Cmd.none
            )



-- TODO
-- VIEW


view model =
    div []
        [ pre [ style "font-size" "xx-large" ]
            [ text """
🂠 26 (+1)

🃞 

🃛

🂠 26 (-1)
"""
            ]
        , button [ onClick ClickedGo ]
            [ text "GO" ]
        ]
