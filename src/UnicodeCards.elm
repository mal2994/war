module UnicodeCards exposing (..)

import Array exposing (Array)
import War exposing (Card, Suit(..))


getCardInUnicode : Card -> Maybe String
getCardInUnicode card =
    case card.suit of
        Hearts ->
            let
                codeStrings =
                    Array.fromList [ "🂱", "🂲", "🂳", "🂴", "🂵", "🂶", "🂷", "🂸", "🂹", "🂺", "🂻", "🂼", "🂽", "🂾" ]
            in
            Array.get card.rank codeStrings

        Spades ->
            let
                codeStrings =
                    Array.fromList [ "🂡", "🂢", "🂣", "🂤", "🂥", "🂦", "🂧", "🂨", "🂩", "🂪", "🂫", "🂬", "🂭", "🂮" ]
            in
            Array.get card.rank codeStrings

        Diamonds ->
            let
                codeStrings =
                    Array.fromList [ "🃁", "🃂", "🃃", "🃄", "🃅", "🃆", "🃇", "🃈", "🃉", "🃊", "🃋", "🃌", "🃍", "🃎" ]
            in
            Array.get card.rank codeStrings

        Clubs ->
            let
                codeStrings =
                    Array.fromList [ "🃑", "🃒", "🃓", "🃔", "🃕", "🃖", "🃗", "🃘", "🃙", "🃚", "🃛", "🃜", "🃝", "🃞" ]
            in
            Array.get card.rank codeStrings



-- shorts, watermelon
