module War exposing (..)

import Browser
import Html exposing (button, div, pre, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }


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
    { players : List Player }



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


init : ( Model, Cmd Msg )
init =
    -- TODO: we need to put real data in the initial model below
    ( { players =
            [ { hand = [ { rank = 1, suit = Hearts } ]
              , score = 0
              }
            , { hand = [ { rank = 1, suit = Hearts } ]
              , score = 0
              }
            ]
      }
    , Cmd.none
    )



-- UPDATE


update msg model =
    model



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
        , button []
            [ text "GO" ]
        ]
