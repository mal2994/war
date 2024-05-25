module UnicodeCards exposing (..)

import Array exposing (Array)
import Types exposing (..)


heartStrings : Array String
heartStrings =
    Array.fromList [ "🂱", "🂲", "🂳", "🂴", "🂵", "🂶", "🂷", "🂸", "🂹", "🂺", "🂻", "🂽", "🂾" ]


spadeStrings : Array String
spadeStrings =
    Array.fromList [ "🂡", "🂢", "🂣", "🂤", "🂥", "🂦", "🂧", "🂨", "🂩", "🂪", "🂫", "🂭", "🂮" ]


diamondStrings : Array String
diamondStrings =
    Array.fromList [ "🃁", "🃂", "🃃", "🃄", "🃅", "🃆", "🃇", "🃈", "🃉", "🃊", "🃋", "🃍", "🃎" ]


clubStrings : Array String
clubStrings =
    Array.fromList [ "🃑", "🃒", "🃓", "🃔", "🃕", "🃖", "🃗", "🃘", "🃙", "🃚", "🃛", "🃝", "🃞" ]



-- anyone know what this is?
-- "🃜",


getCardInUnicode : Maybe Card -> Maybe String
getCardInUnicode card =
    let
        cardGetHelper : Card -> Maybe String
        cardGetHelper myCard =
            case myCard.suit of
                Hearts ->
                    Array.get myCard.rank heartStrings

                Spades ->
                    Array.get myCard.rank spadeStrings

                Diamonds ->
                    Array.get myCard.rank diamondStrings

                Clubs ->
                    Array.get myCard.rank clubStrings
    in
    card
        |> Maybe.andThen cardGetHelper
