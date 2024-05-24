module UnicodeCardsTest exposing (..)

import Expect exposing (..)
import Test exposing (..)
import Types exposing (..)
import UnicodeCards exposing (..)
import War exposing (..)


smokeTest : Test
smokeTest =
    describe "Get a unicode symbol given a playing card"
        [ test "King of Spades character" <|
            \_ ->
                Just { rank = 12, suit = Clubs }
                    |> getCardInUnicode
                    |> Maybe.withDefault "(Nothing)"
                    |> Expect.equal "🃞"
        ]
