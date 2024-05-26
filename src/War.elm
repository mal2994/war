module War exposing (..)

import Browser
import Html exposing (a, button, div, pre, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List exposing (foldl, foldr)
import Maybe exposing (Maybe, withDefault)
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
    if 1 == 1 then
        ( initialModel, initialCmd )

    else
        ( { players =
                ( { hand =
                        [ Card 1 Clubs
                        , Card 0 Diamonds
                        , Card 0 Hearts
                        , Card 1 Spades
                        , Card 2 Diamonds
                        ]
                  , score = 0
                  , topCards = []
                  }
                , { hand =
                        [ Card 0 Clubs
                        , Card 0 Diamonds
                        , Card 0 Hearts
                        , Card 0 Spades
                        , Card 1 Diamonds
                        ]
                  , score = 0
                  , topCards = []
                  }
                )
          }
        , Cmd.none
        )


initialModel : Model
initialModel =
    { players =
        ( { hand = [], score = 0, topCards = [] }
        , { hand = [], score = 0, topCards = [] }
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
                        ( newTopCards0, newTopCards1 ) =
                            if
                                List.head player0.topCards
                                    == List.head player1.topCards
                            then
                                ( c0 :: player0.topCards
                                , c1 :: player1.topCards
                                )

                            else
                                ( [ c0 ], [ c1 ] )

                        ( newScore0, newScore1 ) =
                            let
                                scoreMultipler =
                                    (*) (List.length newTopCards0)
                            in
                            scoreTurn ( Just c0, Just c1 )
                                |> mapBoth scoreMultipler scoreMultipler

                        ( newHand0, newHand1 ) =
                            if c0.rank > c1.rank then
                                ( (player0.hand |> List.drop 1)
                                    ++ newTopCards0
                                    ++ newTopCards1
                                , player1.hand |> List.drop 1
                                )

                            else if c0.rank < c1.rank then
                                ( player0.hand |> List.drop 1
                                , (player1.hand |> List.drop 1)
                                    ++ newTopCards0
                                    ++ newTopCards1
                                )

                            else
                                ( List.drop 1 player0.hand
                                , List.drop 1 player1.hand
                                )
                    in
                    ( { model
                        | players =
                            ( { score = newScore0
                              , hand = newHand0
                              , topCards = newTopCards0
                              }
                            , { score = newScore1
                              , hand = newHand1
                              , topCards = newTopCards1
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


scoreTurn : ( Maybe Card, Maybe Card ) -> ( Int, Int )
scoreTurn cards =
    case cards of
        ( Just c0, Just c1 ) ->
            let
                ( r0, r1 ) =
                    mapBoth .rank .rank ( c0, c1 )
            in
            if r0 < r1 then
                ( -1, 1 )

            else if r0 > r1 then
                ( 1, -1 )

            else
                ( 0, 0 )

        ( _, _ ) ->
            ( 0, 0 )


exchangeHand : ( Card, Card ) -> ( List Card -> List Card, List Card -> List Card )
exchangeHand cards =
    let
        ( r0, r1 ) =
            cards |> mapBoth .rank .rank
    in
    if r0 < r1 then
        ( List.drop 1, preappendList [ second cards ] << rotateList )

    else if r0 > r1 then
        ( preappendList [ first cards ] << rotateList, List.drop 1 )

    else
        ( List.drop 1, List.drop 1 )


rotateList : List a -> List a
rotateList list =
    case List.head list of
        Just el ->
            List.drop 1 list ++ [ el ]

        Nothing ->
            list


preappendList : List a -> List a -> List a
preappendList list0 list1 =
    list1 ++ list0



-- VIEW


view : Model -> Html.Html Msg
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
    let
        scoreFormatter : Int -> String
        scoreFormatter n =
            if n > 0 then
                "+" ++ String.fromInt n

            else
                String.fromInt n

        topCardFolding : Card -> String -> String
        topCardFolding card acc =
            getCardInUnicode (Just card)
                |> withDefault ""
                |> (++) (acc ++ " ")
    in
    [ "🂠 "
        ++ (List.length p.hand |> String.fromInt)
        ++ " ("
        ++ scoreFormatter p.score
        ++ ")"
    , List.foldr topCardFolding "" p.topCards
    ]
