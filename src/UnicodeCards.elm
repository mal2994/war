module UnicodeCards exposing (..)

import Array exposing (Array)
import War exposing (Card, Suit(..))


heartStrings : Array String
heartStrings =
    Array.fromList [ "🂱", "🂲", "🂳", "🂴", "🂵", "🂶", "🂷", "🂸", "🂹", "🂺", "🂻", "🂼", "🂽", "🂾" ]


spadeStrings : Array String
spadeStrings =
    Array.fromList [ "🂡", "🂢", "🂣", "🂤", "🂥", "🂦", "🂧", "🂨", "🂩", "🂪", "🂫", "🂬", "🂭", "🂮" ]


diamondStrings : Array String
diamondStrings =
    Array.fromList [ "🃁", "🃂", "🃃", "🃄", "🃅", "🃆", "🃇", "🃈", "🃉", "🃊", "🃋", "🃌", "🃍", "🃎" ]


clubStrings : Array String
clubStrings =
    Array.fromList [ "🃑", "🃒", "🃓", "🃔", "🃕", "🃖", "🃗", "🃘", "🃙", "🃚", "🃛", "🃜", "🃝", "🃞" ]


getCardInUnicode : Card -> Maybe String
getCardInUnicode card =
    case card.suit of
        Hearts ->
            Array.get card.rank heartStrings

        Spades ->
            Array.get card.rank spadeStrings

        Diamonds ->
            Array.get card.rank diamondStrings

        Clubs ->
            Array.get card.rank clubStrings
