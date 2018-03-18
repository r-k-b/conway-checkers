module Main exposing (..)

import Html exposing (Html, button, text, input, code, div, span, br)
import Html.Events
import Set exposing (Set)
import List.Extra
import Html.Attributes exposing (attribute, style)
import Viewport exposing (Cell, Viewport, cellX, cellY, viewportFromCells)


main =
    Html.program
        { init = ( defaults, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { defaultCell : CellState
    , flippedCells : Set Cell
    , cellsInView : Viewport
    , gameMode : GameMode
    , selectedCell : Maybe ( Cell, LegalMoves )
    }


type CellState
    = Empty
    | Filled



-- What kind of type would make it impossible to select a cell that doesn't contain a chip?


type GameMode
    = NoneSelected
    | CellSelected Cell
    | Pregame


defaults : Model
defaults =
    { defaultCell = Empty
    , flippedCells = Set.fromList [ ( 0, 0 ), ( 0, 1 ), ( 1, 0 ), ( 2, 0 ) ]
    , cellsInView = viewportFromCells ( 8, 8 ) ( -8, -8 )
    , gameMode = NoneSelected
    , selectedCell = Nothing
    }


type Msg
    = SelectCell Cell
    | DeselectAll
    | MoveChip Cell Direction
    | ExitPregame
    | ToggleDefaultFill


type Direction
    = Up -- = -y
    | Down -- = +y
    | Left -- = -x
    | Right -- = +x


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectCell cell ->
            case model.gameMode of
                Pregame ->
                    if cell |> isAboveGoal then
                        ( model, Cmd.none )
                    else
                        ( { model | flippedCells = toggle model.flippedCells cell }, Cmd.none )

                NoneSelected ->
                    if cell |> hasChip model.defaultCell model.flippedCells then
                        ( { model
                            | selectedCell = Just ( cell, legalMovesForCell model.defaultCell model.flippedCells cell )
                            , gameMode = CellSelected cell
                          }
                        , Cmd.none
                        )
                    else
                        ( { model | selectedCell = Nothing }
                        , Cmd.none
                        )

                CellSelected oldCell ->
                    if cell |> hasChip model.defaultCell model.flippedCells then
                        ( { model
                            | selectedCell = Just ( cell, legalMovesForCell model.defaultCell model.flippedCells cell )
                            , gameMode = CellSelected cell
                          }
                        , Cmd.none
                        )
                    else
                        ( { model | selectedCell = Nothing }
                        , Cmd.none
                        )

        DeselectAll ->
            ( { model | selectedCell = Nothing }, Cmd.none )

        MoveChip cell direction ->
            Debug.crash "todo"

        ExitPregame ->
            ( { model | gameMode = NoneSelected }, Cmd.none )

        ToggleDefaultFill ->
            ( { model | defaultCell = flipState model.defaultCell }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        ([ viewBoard model
         , viewGameMode model.gameMode
         ]
            ++ case model.gameMode of
                Pregame ->
                    [ viewFillToggler model.gameMode model.defaultCell
                    ]

                _ ->
                    [ viewScore model.defaultCell model.flippedCells ]
        )


viewGameMode : GameMode -> Html Msg
viewGameMode mode =
    div []
        [ case mode of
            Pregame ->
                button [ Html.Events.onClick ExitPregame ] [ text "Begin Game" ]

            NoneSelected ->
                text "Select a chip to move."

            CellSelected _ ->
                text "Make a move with this chip."
        ]


viewFillToggler : GameMode -> CellState -> Html Msg
viewFillToggler mode state =
    case mode of
        Pregame ->
            span
                []
                [ text "Starting with the board: "
                , button [ Html.Events.onClick ToggleDefaultFill ]
                    [ text <|
                        case state of
                            Empty ->
                                "Empty"

                            Filled ->
                                "Filled"
                    ]
                ]

        _ ->
            span [] []


viewBoard : Model -> Html Msg
viewBoard model =
    let
        unfolded =
            unfoldBoard model.defaultCell model.flippedCells model.cellsInView
    in
        div
            [ style
                [ ( "font-family", "monospace" )
                , ( "user-select", "none" )
                , ( "zoom", "2" )
                ]
            ]
            (List.map (viewRow model.selectedCell) unfolded)


cellSize : String
cellSize =
    "2em"


viewRow : Maybe ( Cell, LegalMoves ) -> List ( Cell, CellState ) -> Html Msg
viewRow selectedCell row =
    div
        [ style
            [ ( "margin", "0" )
            , ( "padding", "0" )
            , ( "height", cellSize )
            , ( "white-space", "nowrap" )
            ]
        ]
        (List.map (viewCell selectedCell) row)


viewCell : Maybe ( Cell, LegalMoves ) -> ( Cell, CellState ) -> Html Msg
viewCell selectedCell ( address, cell ) =
    let
        isBlack =
            ((address |> cellX) + (address |> cellY)) % 2 == 0

        inner =
            case cell of
                Empty ->
                    []

                Filled ->
                    [ div
                        [ style
                            [ ( "position", "absolute" )
                            , ( "top", "0" )
                            , ( "bottom", "0" )
                            , ( "left", "0" )
                            , ( "right", "0" )
                            ]
                        ]
                        [ text "⛂"
                        ]
                    ]

        background =
            if isBlack then
                [ ( "background-color", "black" ), ( "color", "white" ) ]
            else
                []

        isSelected =
            Maybe.map (Tuple.first >> (==) address) selectedCell
                |> Maybe.withDefault False

        selectedStyle =
            if isSelected then
                [ ( "border", "2px solid green" ) ]
            else
                []

        moveIndicators : List (Html Msg)
        moveIndicators =
            if isSelected then
                case selectedCell of
                    Nothing ->
                        []

                    Just ( _, legalMoves ) ->
                        (viewMoveIndicator address legalMoves.up Up)
                            ++ (viewMoveIndicator address legalMoves.down Down)
                            ++ (viewMoveIndicator address legalMoves.left Left)
                            ++ (viewMoveIndicator address legalMoves.right Right)
            else
                []

        cellStyle =
            style
                ([ ( "display", "inline-block" )
                 , ( "position", "relative" )
                 , ( "width", cellSize )
                 , ( "height", cellSize )
                 , ( "overflow", "visible" )
                 , ( "text-align", "center" )
                 , ( "margin", "0" )
                 , ( "padding", "0" )
                 , ( "box-sizing", "border-box" )
                 , ( "opacity"
                   , if isSelected then
                        "1"
                     else
                        "0.2"
                   )
                 ]
                    ++ background
                    ++ selectedStyle
                )
    in
        div
            [ cellStyle
            , attribute "data-cell-address" (address |> toString)
            , Html.Events.onClick (SelectCell address)
            ]
            (inner ++ moveIndicators)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


flipState : CellState -> CellState
flipState state =
    case state of
        Empty ->
            Filled

        Filled ->
            Empty


unfoldRow : CellState -> Int -> Set Cell -> Viewport -> Int -> Maybe ( ( Cell, CellState ), Int )
unfoldRow defaultState y flippedCells viewport nextX =
    if nextX > (Viewport.maxCell viewport |> cellX) then
        Nothing
    else
        let
            address =
                ( nextX, y )
        in
            if Set.member ( nextX, y ) flippedCells then
                Just
                    ( ( address, flipState defaultState )
                    , nextX + 1
                    )
            else
                Just
                    ( ( address, defaultState )
                    , nextX + 1
                    )


unfoldRows : CellState -> Set Cell -> Viewport -> Int -> Maybe ( List ( Cell, CellState ), Int )
unfoldRows defaultState flippedCells viewport nextY =
    if nextY > (Viewport.maxCell viewport |> cellY) then
        Nothing
    else
        let
            rowDefaultState =
                if nextY < 0 then
                    Empty
                else
                    defaultState
        in
            Just
                ( List.Extra.unfoldr (unfoldRow rowDefaultState nextY flippedCells viewport) (Viewport.minCell viewport |> cellX)
                , nextY + 1
                )


unfoldBoard : CellState -> Set Cell -> Viewport -> List (List ( Cell, CellState ))
unfoldBoard defaultState flippedCells viewport =
    List.Extra.unfoldr (unfoldRows defaultState flippedCells viewport) (Viewport.minCell viewport |> cellY)


isAboveGoal : Cell -> Bool
isAboveGoal cell =
    (cell |> cellY) < 0


toggle : Set Cell -> Cell -> Set Cell
toggle old address =
    if old |> Set.member address then
        Set.remove address old
    else
        Set.insert address old


getScore : Set Cell -> Int
getScore flippedCells =
    let
        highest : Cell -> Int -> Int
        highest cell prior =
            min (cell |> cellY) prior
    in
        Set.foldr highest 0 flippedCells
            |> (*) -1


viewScore : CellState -> Set Cell -> Html Msg
viewScore defaultState flippedCells =
    div []
        [ text <| "Highest row reached: " ++ (getScore flippedCells |> toString)
        , br [] []
        , text <| "Chips remaining: " ++ (countChipsRemaining defaultState flippedCells |> toString)
        ]


hasChip : CellState -> Set Cell -> Cell -> Bool
hasChip defaultState flippedCells address =
    let
        isFlipped =
            Set.member address flippedCells
    in
        if isAboveGoal address then
            isFlipped
        else
            case defaultState of
                Empty ->
                    isFlipped

                Filled ->
                    not isFlipped


infinity : Float
infinity =
    (1 / 0)


countChipsRemaining : CellState -> Set Cell -> Float
countChipsRemaining defaultState flippedCells =
    case defaultState of
        Filled ->
            infinity

        Empty ->
            Set.size flippedCells |> toFloat


type alias LegalMoves =
    { up : Bool
    , down : Bool
    , left : Bool
    , right : Bool
    }


up : Int -> Cell -> Cell
up by cell =
    ( cell |> cellX, (cell |> cellY) - 1 )


down : Int -> Cell -> Cell
down by cell =
    up (by * -1) cell


left : Int -> Cell -> Cell
left by cell =
    ( (cell |> cellX) - 1, cell |> cellY )


right : Int -> Cell -> Cell
right by cell =
    left (by * -1) cell


{-| todo: write tests for legalMovesForCell
-}
legalMovesForCell : CellState -> Set Cell -> Cell -> LegalMoves
legalMovesForCell defaultState flippedCells address =
    let
        chipAt =
            hasChip defaultState flippedCells

        centralChip =
            chipAt address

        x =
            [ Debug.log <| toString <| centralChip
            ]
    in
        { up = centralChip && chipAt (address |> up 1) && (not <| chipAt (address |> up 2))
        , down = centralChip && chipAt (address |> down 1) && (not <| chipAt (address |> down 2))
        , left = centralChip && chipAt (address |> left 1) && (not <| chipAt (address |> left 2))
        , right = centralChip && chipAt (address |> right 1) && (not <| chipAt (address |> right 2))
        }


viewMoveIndicator : Cell -> Bool -> Direction -> List (Html Msg)
viewMoveIndicator cell legal direction =
    let
        events =
            if legal then
                [ Html.Events.onClick <| MoveChip cell direction ]
            else
                []

        opacity =
            if legal then
                "1"
            else
                "0.2"

        transform =
            case direction of
                Up ->
                    "translateY(-100%)"

                Down ->
                    "translateY(100%) rotate(180deg)"

                Left ->
                    "translateX(-100%) rotate(270deg)"

                Right ->
                    "translateX(100%) rotate(90deg)"

        bgc =
            if legal then
                "green"
            else
                "red"
    in
        [ div
            (events
                ++ [ style
                        [ ( "position", "absolute" )
                        , ( "top", "0" )
                        , ( "bottom", "0" )
                        , ( "left", "0" )
                        , ( "right", "0" )
                        , ( "transform", transform )
                        , ( "mix-blend-mode", "difference" )
                        , ( "color", "blue" )
                        , ( "background-color", bgc )
                        , ( "opacity", opacity )
                        ]
                   ]
            )
            [ text "↑" ]
        ]
