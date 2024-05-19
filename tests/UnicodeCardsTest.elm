module UnicodeCardsTest exposing (..)

import Expect exposing (..)
import Test exposing (..)
import UnicodeCards exposing (..)
import War exposing (Suit(..))


smokeTest : Test
smokeTest =
    describe "Get a unicode symbol given a playing card"
        [ test "King of Spades character" <|
            \_ ->
                getCardInUnicode { rank = 13, suit = Clubs }
                    |> Maybe.withDefault "(Nothing)"
                    |> Expect.equal "🃞"
        ]
