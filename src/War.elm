module War exposing (..)

import Browser
import Html exposing (button, div, pre, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List exposing (foldl, foldr)
import Maybe exposing (withDefault)
import Random
import Random.List
import Tuple exposing (first, mapBoth, second)
import Types exposing (..)
import UnicodeCards exposing (getCardInUnicode)



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


type Msg
    = ClickedGo
    | GotRandomDeck (List Card)



-- MODEL
-- moved some things to `Types.elm` to fix circular dependency of Main/UnicodeCards


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
        (List.range 0 12)
        |> List.concat


init : a -> ( Model, Cmd Msg )
init _ =
    ( initialModel, initialCmd )


initialModel : Model
initialModel =
    let
        -- TODO: remove this temp card as it is just for testing mockup
        temp_card =
            { rank = 13, suit = Clubs }
    in
    { players =
        ( { hand = [ temp_card ], score = 0 }
        , { hand = [ temp_card ], score = 0 }
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
                    let
                        ( newScore0, newScore1 ) =
                            scoreTurn ( c0, c1 )

                        ( newHand0, newHand1 ) =
                            exchangeHand ( c0, c1 )
                    in
                    ( { model
                        | players =
                            ( { score = newScore0
                              , hand = newHand0 player0.hand
                              }
                            , { score = newScore1
                              , hand = newHand1 player1.hand
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
            [ text <| viewPlayers model ]
        , button [ onClick ClickedGo ]
            [ text "GO" ]
        ]


viewPlayers : Model -> String
viewPlayers model =
    let
        appendWithInnerNewlines : String -> String -> String
        appendWithInnerNewlines a b =
            a ++ "\n\n" ++ b
    in
    String.append
        (first model.players
            |> viewPlayerHelper
            |> foldr appendWithInnerNewlines ""
        )
        (second model.players
            |> viewPlayerHelper
            |> foldl appendWithInnerNewlines ""
        )


viewPlayerHelper : Player -> List String
viewPlayerHelper p =
    [ "🂠 "
        ++ (List.length p.hand |> String.fromInt)
        ++ " ("
        ++ (p.score |> String.fromInt)
        ++ ")"
    , List.head p.hand
        |> getCardInUnicode
        |> withDefault "(Nothing)"
    ]
