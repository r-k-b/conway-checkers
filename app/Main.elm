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
    { defaultCell = Empty
    , flippedCells = Set.fromList [ ( 0, 0 ) ]
    , cellsInView = viewportFromCells ( 8, 8 ) ( -8, -8 )
    , moveState = NoneSelected
    }


type Msg
    = NoAction


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


viewRow : List CellState -> Html Msg
viewRow row =
    div [] (List.map viewCell row)


viewCell : CellState -> Html Msg
viewCell cell =
    let
        inner =
            case cell of
                Empty ->
                    "."

                Filled ->
                    "⛂"
    in
        div [ style [ ( "display", "inline-block" ), ( "width", "1em" ), ( "text-align", "center" ) ] ] [ text inner ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


flip : CellState -> CellState
flip state =
    case state of
        Empty ->
            Filled

        Filled ->
            Empty


unfoldRow : CellState -> Int -> Set Cell -> Viewport -> Int -> Maybe ( CellState, Int )
unfoldRow defaultState y flippedCells viewport nextX =
    if nextX > (Viewport.maxCell viewport |> cellX) then
        Nothing
    else if Set.member ( nextX, y ) flippedCells then
        Just ( flip defaultState, nextX + 1 )
    else
        Just ( defaultState, nextX + 1 )


unfoldRows : CellState -> Set Cell -> Viewport -> Int -> Maybe ( List CellState, Int )
unfoldRows defaultState flippedCells viewport nextY =
    if nextY > (Viewport.maxCell viewport |> cellY) then
        Nothing
    else
        Just
            ( List.Extra.unfoldr (unfoldRow defaultState nextY flippedCells viewport) (Viewport.minCell viewport |> cellX)
            , nextY + 1
            )


unfoldBoard : CellState -> Set Cell -> Viewport -> List (List CellState)
unfoldBoard defaultState flippedCells viewport =
    List.Extra.unfoldr (unfoldRows defaultState flippedCells viewport) (Viewport.minCell viewport |> cellY)
