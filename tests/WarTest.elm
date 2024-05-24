module WarTest exposing (..)

import Expect exposing (..)
import Test exposing (..)
import Tuple exposing (mapBoth)
import Types exposing (Card, Model, Suit(..))
import War exposing (..)


updatedModel : Model
updatedModel =
    initialModel
        |> update (GotRandomDeck createDeck)
        |> Tuple.first


testInit : Test
testInit =
    describe "Initializing the program." <|
        [ test "Dealing the deck should give 2 players 26 cards each." <|
            \_ ->
                updatedModel.players
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


testFirstTurn : Test
testFirstTurn =
    let
        ( newModel, _ ) =
            update ClickedGo updatedModel
    in
    describe "Updating model after first turn is done." <|
        [ test "One player gets -1, other player gets +1" <|
            \_ ->
                newModel.players
                    |> mapBoth .score .score
                    |> Expect.equal ( -1, 1 )
        , test "Number of cards with each player changes." <|
            \_ ->
                newModel.players
                    |> mapBoth .hand .hand
                    |> mapBoth List.length List.length
                    |> Expect.equal ( 25, 27 )
        , todo "Two new cards in play get shown."
        , todo "Tiebreaker round has score greater than 1."
        , test "Viewing the initial model looks like this." <|
            \_ ->
                viewPlayers updatedModel
                    |> Expect.equal """🂠 26 (0)

🂲

🃘

🂠 26 (0)

"""
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


testGameOver : Test
testGameOver =
    todo "Go button should not do anything"
