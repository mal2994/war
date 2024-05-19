module War exposing (..)

import Browser
import Html exposing (button, div, pre, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import Random.List
import Tuple exposing (mapBoth)



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
        ( player1, player2 ) =
            model.players
    in
    case msg of
        ClickedGo ->
            let
                ( c0, c1 ) =
                    model.players
                        |> mapBoth .hand .hand
                        |> mapBoth List.head List.head
            in
            case ( c0, c1 ) of
                ( Nothing, _ ) ->
                    ( model, Cmd.none )

                ( _, Nothing ) ->
                    ( model, Cmd.none )

                ( _, _ ) ->
                    ( { model
                        | players =
                            ( { score = 1
                              , hand = player1.hand
                              }
                            , { score = 1
                              , hand = player1.hand
                              }
                            )
                      }
                    , Cmd.none
                    )

        GotRandomDeck d ->
            ( { model
                | players =
                    ( { player1 | hand = List.take 26 d }
                    , { player2 | hand = List.drop 26 d }
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
        ( 0, 1 )

    else if r0 > r1 then
        ( 1, 0 )

    else
        ( 0, 0 )



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
