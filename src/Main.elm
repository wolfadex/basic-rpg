module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Maybe exposing (Maybe(..))


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = Playing Game
    | Won


type alias Game =
    { entities : List Entity
    , player : Entity
    , tempName : String
    , adventureLog : List String
    }


type EntityType
    = Player
    | Enemy
    | Portal


type alias Entity =
    { name : String
    , position : Position
    , type_ : EntityType
    , health : Int
    }


type alias Position =
    { x : Int }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialGame
    , Cmd.none
    )


initialGame : Model
initialGame =
    Playing
        { entities =
            [ { name = "Bandit"
              , position = { x = -1 }
              , type_ = Enemy
              , health = 1
              }
            , { name = "Cow"
              , position = { x = 2 }
              , type_ = Enemy
              , health = 2
              }
            , { name = "Escape"
              , position = { x = 4 }
              , type_ = Portal
              , health = 0
              }
            ]
        , player =
            { name = "Carl"
            , position = { x = 0 }
            , type_ = Player
            , health = 10
            }
        , tempName = "Carl"
        , adventureLog =
            [ "You awake in a world of a single dimension." ]
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = NewName String
    | Move Int
    | SetName
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Restart, _ ) ->
            ( initialGame, Cmd.none )

        ( Move amount, Playing game ) ->
            let
                player =
                    game.player

                position =
                    player.position

                newPosition =
                    { position | x = position.x + amount }
            in
            case find (\ent -> ent.position == newPosition) game.entities of
                Nothing ->
                    ( Playing
                        { game
                            | player = { player | position = newPosition }
                            , adventureLog =
                                addAdventureLog
                                    ("You walk to the "
                                        ++ (if amount > 0 then
                                                "right"

                                            else
                                                "left"
                                           )
                                    )
                                    game.adventureLog
                        }
                    , Cmd.none
                    )

                Just ent ->
                    case ent.type_ of
                        Player ->
                            ( Playing game, Cmd.none )

                        Portal ->
                            ( Won, Cmd.none )

                        Enemy ->
                            let
                                ( newEntities, logMessage ) =
                                    List.foldr
                                        (\e ( newEnts, logMsg ) ->
                                            if e.position == newPosition then
                                                let
                                                    newEnt =
                                                        { e | health = e.health - 1 }
                                                in
                                                if newEnt.health < 1 then
                                                    ( newEnts, Just ("You defeat the " ++ e.name) )

                                                else
                                                    ( newEnt :: newEnts, Just ("You attack the " ++ e.name) )

                                            else
                                                ( e :: newEnts, logMsg )
                                        )
                                        ( [], Nothing )
                                        game.entities
                            in
                            ( Playing
                                { game
                                    | entities = newEntities
                                    , adventureLog =
                                        addAdventureLog
                                            (Maybe.withDefault "Carl shows up to help you find your way" logMessage)
                                            game.adventureLog
                                }
                            , Cmd.none
                            )

        ( NewName name, Playing game ) ->
            ( Playing { game | tempName = name }
            , Cmd.none
            )

        ( SetName, Playing game ) ->
            let
                player =
                    game.player
            in
            ( Playing
                { game
                    | player = { player | name = game.tempName }
                    , adventureLog =
                        addAdventureLog
                            ("You change your name to: " ++ game.tempName)
                            game.adventureLog
                }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


addAdventureLog : String -> List String -> List String
addAdventureLog newLog logs =
    newLog
        :: logs
        |> List.take 200


find : (a -> Bool) -> List a -> Maybe a
find pred list =
    case list of
        [] ->
            Nothing

        head :: rest ->
            if pred head then
                Just head

            else
                find pred rest


view : Model -> Html Msg
view model =
    layout [ padding 16 ] <|
        case model of
            Playing game ->
                viewBody game

            Won ->
                column
                    [ centerX, Font.size 32 ]
                    [ text "You found a way out!"
                    , Input.button
                        [ centerX
                        , Border.solid
                        , Border.width 3
                        , Border.rounded 7
                        , paddingXY 16 8
                        ]
                        { label = text "Restart"
                        , onPress = Just Restart
                        }
                    ]


viewBody : Game -> Element Msg
viewBody game =
    let
        before =
            game.entities
                |> List.filter (\{ position } -> position.x == game.player.position.x - 1)
                |> List.head
                |> (\maybeEntity ->
                        case maybeEntity of
                            Nothing ->
                                viewNoEntity

                            Just entity ->
                                viewEntity entity
                   )

        after =
            game.entities
                |> List.filter (\{ position } -> position.x == game.player.position.x + 1)
                |> List.head
                |> (\maybeEntity ->
                        case maybeEntity of
                            Nothing ->
                                viewNoEntity

                            Just entity ->
                                viewEntity entity
                   )
    in
    column
        [ spacing 16, centerX ]
        [ text "1D World"
            |> el [ Font.size 32, Font.underline, centerX ]
        , row
            [ spacing 16, centerX ]
            [ Input.text
                []
                { label = Input.labelLeft [] (text "Name")
                , onChange = NewName
                , placeholder = Nothing
                , text = game.tempName
                }
            , Input.button
                [ Border.solid
                , Border.width 3
                , Border.rounded 7
                , paddingXY 16 8
                ]
                { label = text "Set Name"
                , onPress = Just SetName
                }
            ]
        , row
            [ spacing 8, centerX ]
            [ Input.button
                []
                { label = before
                , onPress = Just (Move -1)
                }
            , viewEntity game.player
            , Input.button
                []
                { label = after
                , onPress = Just (Move 1)
                }
            ]
        , viewAdventureLog game.adventureLog
        ]


viewAdventureLog : List String -> Element Msg
viewAdventureLog notes =
    notes
        |> List.map ((++) "• " >> text)
        |> column [ spacing 4 ]


viewNoEntity : Element Msg
viewNoEntity =
    column
        [ spacing 4 ]
        [ el
            [ padding 8
            , Border.solid
            , Border.width 3
            , Border.rounded 7
            ]
            (text "    ")
        , text ""
        ]


viewEntity : Entity -> Element Msg
viewEntity { name, type_, health } =
    column
        [ spacing 4 ]
        [ text name
            |> el
                [ padding 8
                , width fill
                , Border.solid
                , Border.width 3
                , Border.rounded 7
                , Border.color (rgb 0 0 0)
                , Background.color <|
                    case type_ of
                        Player ->
                            rgb 0.5 1 0.5

                        Portal ->
                            rgb 0 0.75 0.75

                        Enemy ->
                            rgb 1 0 0
                , Font.color <|
                    case type_ of
                        Player ->
                            rgb 0 0 0

                        Portal ->
                            rgb 0 0 0

                        Enemy ->
                            rgb 1 1 1
                ]
        , health
            |> String.fromInt
            |> (++) "HP: "
            |> text
            |> el [ centerX ]
        ]
