module UnicodeCards exposing (..)

import Array exposing (Array)
import Types exposing (..)


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


getCardInUnicode : Maybe Card -> Maybe String
getCardInUnicode card =
    -- TODO: Any way to not allow null here?
    case card of
        Nothing ->
            Just "(Nothing)"

        Just justCard ->
            case justCard.suit of
                Hearts ->
                    Array.get justCard.rank heartStrings

                Spades ->
                    Array.get justCard.rank spadeStrings

                Diamonds ->
                    Array.get justCard.rank diamondStrings

                Clubs ->
                    Array.get justCard.rank clubStrings
