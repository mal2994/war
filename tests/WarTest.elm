module WarTest exposing (..)

import Expect exposing (..)
import Test exposing (..)
import War exposing (..)


testInit : Test
testInit =
    describe "Initializing the program." <|
        [ test "Dealing the deck should give 2 players 26 cards each." <|
            \_ ->
                Expect.all
                    [ Expect.equal
                        (List.length
                            (Tuple.first initialModel.players).hand
                        )
                    , Expect.equal
                        (List.length
                            (Tuple.second initialModel.players).hand
                        )
                    ]
                    26
        , test "The cards dealt should have been shuffled." <|
            -- sort
            \_ ->
                Expect.fail "Need to write this test tomorrow..."
        , todo "Game begins with both players at +0"
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
