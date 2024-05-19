module War exposing (..)

import Browser
import Html exposing (button, div, pre, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


type Msg
    = ClickedGo
    | GotRandomCard Int



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
    ( initialModel
    , Cmd.none
    )


initialModel : Model
initialModel =
    let
        deck =
            createDeck
    in
    { players =
        ( { hand = List.take 26 deck
          , score = 0
          }
        , { hand = List.drop 26 deck
          , score = 0
          }
        )
    }



-- UPDATE


update msg model =
    case msg of
        ClickedGo ->
            ( model, Cmd.none )

        GotRandomCard n ->
            ( model, Cmd.none )



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
