module Types exposing (..)

-- MODEL


type Suit
    = Hearts
    | Diamonds
    | Clubs
    | Spades


type alias Card =
    { rank : Int
    , suit : Suit
    }


type alias Player =
    { hand : List Card
    , score : Int
    , topCards : List Card
    }


type alias Model =
    { players : ( Player, Player )
    }
