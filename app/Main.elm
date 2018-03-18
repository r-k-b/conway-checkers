module Main exposing (..)

import Html exposing (Html, text, input, code, div)
import Set exposing (Set)
import List.Extra
import Html.Attributes exposing (style)
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
    , moveState : MoveState
    }


type CellState
    = Empty
    | Filled



-- What kind of type would make it impossible to select a cell that doesn't contain a chip?


type MoveState
    = NoneSelected
    | CellSelected Cell


defaults : Model
defaults =
    { defaultCell = Filled
    , flippedCells = Set.fromList [ ( 0, 0 ) ]
    , cellsInView = viewportFromCells ( 8, 8 ) ( -8, -8 )
    , moveState = NoneSelected
    }


type Msg
    = NoAction
    | SelectCell Cell
    | MoveChip Cell Direction


type Direction
    = Up -- = -y
    | Down -- = +y
    | Left -- = -x
    | Right -- = +x


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        unfolded =
            unfoldBoard model.defaultCell model.flippedCells model.cellsInView
    in
        div [ style [ ( "font-family", "monospace" ) ] ] (List.map viewRow unfolded)


cellSize : String
cellSize =
    "2em"


viewRow : List ( Cell, CellState ) -> Html Msg
viewRow row =
    div
        [ style
            [ ( "margin", "0" )
            , ( "padding", "0" )
            , ( "height", cellSize )
            ]
        ]
        (List.map viewCell row)


viewCell : ( Cell, CellState ) -> Html Msg
viewCell ( address, cell ) =
    let
        isBlack =
            ((address |> cellX) + (address |> cellY)) % 2 == 0

        inner =
            case cell of
                Empty ->
                    " "

                Filled ->
                    "⛂"

        background =
            if isBlack then
                [ ( "background-color", "black" ), ( "color", "white" ) ]
            else
                []
    in
        div
            [ style
                ([ ( "display", "inline-block" )
                 , ( "width", cellSize )
                 , ( "height", cellSize )
                 , ( "overflow", "hidden" )
                 , ( "text-align", "center" )
                 , ( "margin", "0" )
                 , ( "padding", "0" )
                 ]
                    ++ background
                )
            , Html.Attributes.attribute "data-cell-address" (address |> toString)
            ]
            [ text inner ]


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
