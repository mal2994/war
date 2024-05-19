module War exposing (..)

import Browser
import Html exposing (button, div, pre, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import Random.List
import Tuple exposing (first, mapBoth, second)



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


type Msg
    = ClickedGo
    | GotRandomDeck (List Card)



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
    }


type alias Model =
    { players : ( Player, Player )
    }



-- init : Model


createDeck : List Card
createDeck =
    List.map
        (\rank ->
            List.map
                (\suit ->
                    { rank = rank, suit = suit }
                )
                [ Hearts, Diamonds, Clubs, Spades ]
        )
        (List.range 1 13)
        |> List.concat


init : a -> ( Model, Cmd Msg )
init _ =
    ( initialModel, initialCmd )


initialModel : Model
initialModel =
    { players =
        ( { hand = [], score = 0 }
        , { hand = [], score = 0 }
        )
    }


initialCmd : Cmd Msg
initialCmd =
    Random.generate GotRandomDeck (Random.List.shuffle createDeck)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( player0, player1 ) =
            model.players
    in
    case msg of
        ClickedGo ->
            let
                ( card0, card1 ) =
                    model.players
                        |> mapBoth .hand .hand
                        |> mapBoth List.head List.head
            in
            case ( card0, card1 ) of
                ( Nothing, _ ) ->
                    ( model, Cmd.none )

                ( _, Nothing ) ->
                    ( model, Cmd.none )

                ( Just c0, Just c1 ) ->
                    ( { model
                        | players =
                            ( { score = scoreTurn ( c0, c1 ) |> first
                              , hand = (exchangeHand ( c0, c1 ) |> first) player0.hand
                              }
                            , { score = scoreTurn ( c0, c1 ) |> second
                              , hand = (exchangeHand ( c0, c1 ) |> second) player1.hand
                              }
                            )
                      }
                    , Cmd.none
                    )

        GotRandomDeck d ->
            ( { model
                | players =
                    ( { player0 | hand = List.take 26 d }
                    , { player1 | hand = List.drop 26 d }
                    )
              }
            , Cmd.none
            )


scoreTurn : ( Card, Card ) -> ( Int, Int )
scoreTurn cards =
    let
        ( r0, r1 ) =
            cards |> mapBoth .rank .rank
    in
    if r0 < r1 then
        ( -1, 1 )

    else if r0 > r1 then
        ( 1, -1 )

    else
        ( 0, 0 )


exchangeHand : ( Card, Card ) -> ( List Card -> List Card, List Card -> List Card )
exchangeHand cards =
    let
        ( r0, r1 ) =
            cards |> mapBoth .rank .rank
    in
    if r0 < r1 then
        ( List.drop 1, List.append [ second cards ] )

    else if r0 > r1 then
        ( List.append [ first cards ], List.drop 1 )

    else
        ( List.drop 0, List.drop 0 )



-- TODO
-- VIEW


view model =
    div []
        [ pre [ style "font-size" "xx-large" ]
            [ text """
🂠 26 (+1)

🃞 

🃛

🂠 26 (-1)
"""
            ]
        , button [ onClick ClickedGo ]
            [ text "GO" ]
        ]
