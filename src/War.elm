module War exposing (..)

import Browser
import Html exposing (button, div, pre, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)



-- MAIN
-- main =
-- Browser.sandbox { init = init, update = update, view = view }

-- main =
    -- Html.text "Hello"



main =
    view {}

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
    { cardsRemaining : Int
    , cardPlayed : Card
    , isTurnWon : Bool
    , hand : List Card
    }


type alias Model =
    { players : List Player }



-- init : Model
-- init =
--     let
--         dummy : Int
--         dummy =
--             1
--     in
--     { players =
--         [ {}
--         , {}
--         ]
--     }
-- UPDATE


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
