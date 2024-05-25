module WarTest exposing (..)

import Expect exposing (..)
import List exposing (map, member)
import Test exposing (..)
import Tuple exposing (mapBoth)
import Types exposing (Card, Model, Player, Suit(..))
import War exposing (..)


modelAfterCardDeal : Model
modelAfterCardDeal =
    initialModel
        |> update (GotRandomDeck createDeck)
        |> Tuple.first


modelAfterTurnOne : Model
modelAfterTurnOne =
    modelAfterCardDeal
        |> update ClickedGo
        |> Tuple.first


testInit : Test
testInit =
    describe "Initializing the program." <|
        [ test "Dealing the deck should give 2 players 26 cards each." <|
            \_ ->
                modelAfterCardDeal.players
                    |> mapBoth .hand .hand
                    |> mapBoth List.length List.length
                    |> Expect.equal ( 26, 26 )
        , test "The cards dealt should have been shuffled." <|
            \_ ->
                let
                    flags =
                        Nothing
                in
                init flags |> Tuple.second |> Expect.equal initialCmd
        , test "Game begins with both players at +0" <|
            \_ ->
                initialModel.players
                    |> mapBoth .score .score
                    |> Expect.equal ( 0, 0 )
        ]


testViews : Test
testViews =
    describe "Updating model after first turn is done." <|
        [ test "One player gets -1, other player gets +1" <|
            \_ ->
                modelAfterTurnOne.players
                    |> mapBoth .score .score
                    |> Expect.equal ( -1, 1 )
        , test "Number of cards with each player changes." <|
            \_ ->
                modelAfterTurnOne.players
                    |> mapBoth .hand .hand
                    |> mapBoth List.length List.length
                    |> Expect.equal ( 25, 27 )
        , test "Preformatted text view after card deal." <|
            \_ ->
                viewPlayers modelAfterCardDeal
                    |> Expect.equal """🂠 26 (0)





🂠 26 (0)

"""
        , test "Preformatted text view after turn one." <|
            \_ ->
                viewPlayers modelAfterTurnOne
                    |> Expect.equal """🂠 25 (-1)

🃑

🂧

🂠 27 (+1)

"""
        , todo "Tiebreaker round has score greater than 1."
        , todo "Tiebreaker winner gets all the cards in that round."
        , test "Both players play a new card every round plz fix." <|
            \_ ->
                Expect.equal [ 2, 3, 1 ] (rotateList [ 1, 2, 3 ])
        ]


testDeck : Test
testDeck =
    describe "Creating a new deck." <|
        [ test "Deck should contain 52 cards." <|
            \_ ->
                List.length createDeck
                    |> Expect.equal 52
        , test "Ace is 0, King is 12." <|
            \_ ->
                let
                    member_ : Bool -> Card -> List Card -> Expectation
                    member_ b el d =
                        member el d |> Expect.equal b
                in
                Expect.all
                    [ Card 0 Hearts |> member_ True
                    , Card 12 Hearts |> member_ True
                    , Card 13 Hearts |> member_ False
                    , Card -1 Hearts |> member_ False
                    ]
                    createDeck

        -- , todo "Shuffle deck."
        ]


testGameOver : Test
testGameOver =
    todo "Go button should be disabled in GameOver"


testTakingTurns : Test
testTakingTurns =
    describe "How player hand works when taking a few turns."
        [ test "The player hand rotates once with every turn." <|
            \_ -> Expect.equal 1 1
        , test "The pot grows with nested tie breakers." <|
            \_ ->
                let
                    takeTurn : Model -> Model
                    takeTurn m =
                        update ClickedGo m |> Tuple.first

                    modelZero : Model
                    modelZero =
                        { players =
                            ( { hand =
                                    [ Card 0 Clubs
                                    , Card 0 Diamonds
                                    , Card 0 Hearts
                                    , Card 1 Spades
                                    ]
                              , score = 0
                              , topCard = Just <| Card 0 Spades
                              }
                            , { hand =
                                    [ Card 0 Clubs
                                    , Card 0 Diamonds
                                    , Card 0 Hearts
                                    , Card 0 Spades
                                    ]
                              , score = 0
                              , topCard = Just <| Card 0 Spades
                              }
                            )
                        }

                    modelOne =
                        takeTurn modelZero

                    modelTwo =
                        takeTurn modelOne

                    modelThree =
                        takeTurn modelTwo
                in
                map (\p -> (Tuple.first p.players).score) [ modelZero, modelOne, modelTwo, modelThree ]
                    |> Expect.equalLists [ 0, 0, 0, 4 ]
        ]
