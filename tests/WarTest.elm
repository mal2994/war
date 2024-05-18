module WarTest exposing (..)

import Expect exposing (..)
import List exposing (map, range)
import Test exposing (..)
import Tuple exposing (first, mapBoth)
import War exposing (..)


testInit : Test
testInit =
    describe "Initializing the program." <|
        [ test "Dealing the deck should give 2 players 26 cards each." <|
            \_ ->
                Expect.equal ( 26, 26 ) <|
                    mapBoth List.length List.length <|
                        mapBoth .hand .hand <|
                            initialModel.players
        , test "The cards dealt should have been shuffled." <|
            \_ -> Expect.notEqual (List.take 26 createDeck) <| (first initialModel.players).hand
        , test "Game begins with both players at +0" <|
            \_ -> Expect.equal ( 0, 0 ) (mapBoth (\p -> p.score) (\p -> p.score) initialModel.players)
        , todo "Game should have 2 players as a tuple."
        ]


testFirstTurn : Test
testFirstTurn =
    describe "Updating model after first turn is done." <|
        [ todo "One player gets -1, other player gets +1"
        , todo "Number of cards with each player changes."
        , todo "Two cards in play get shown."
        ]


testCreateDeck : Test
testCreateDeck =
    describe "Creating a new deck." <|
        [ test "Deck should contain 52 cards." <|
            \_ ->
                List.length createDeck
                    |> Expect.equal 52
        , test "Deck should contain 1 ace of hearts." <|
            \() ->
                Expect.equal 1
                    (List.filter
                        (\card ->
                            card == { suit = Hearts, rank = 1 }
                        )
                        createDeck
                        |> List.length
                    )
        , test "Deck should contain 1 king of diamonds." <|
            \() ->
                Expect.equal 1
                    (List.filter
                        (\card ->
                            card == { suit = Diamonds, rank = 12 }
                        )
                        createDeck
                        |> List.length
                    )
        , todo "Shuffle deck."
        ]
