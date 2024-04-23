module WarTest exposing (..)

import Expect exposing (..)
import Test exposing (..)
import War exposing (..)


dummySuite : Test
dummySuite =
    test "elm test is working fine" <|
        \_ -> Expect.equal 1 1


testCardDeals : Test
testCardDeals =
    describe "Creating a deck." <|
        [ todo "Create deck."
        , todo "Shuffle deck."
        , todo "Deal deck."
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
        ]



-- createDeck : List Card
-- createDeck =
--     List.concatMap
--         (\rank ->
--             List.map
--                 (\suit ->
--                     { rank = rank, suit = suit }
--                 )
--                 [ Hearts, Diamonds, Clubs, Spades ]
--         )
--         (List.range 1 13)
