module Viewport exposing (Cell, cellX, cellY, maxCell, minCell, viewportFromCells, Viewport)


type alias Cell =
    ( Int, Int )


{-| Opaque type; do not export the constructor.

By being opaque, we can enforce the invariant that the 'min' cell is strictly
closer to -∞ than the 'max' cell on both X and Y axes.
-}
type Viewport
    = Viewport
        { min : Cell
        , max : Cell
        }


cellX : Cell -> Int
cellX =
    Tuple.first


cellY : Cell -> Int
cellY =
    Tuple.second


viewportFromCells : Cell -> Cell -> Viewport
viewportFromCells a b =
    Viewport
        { min =
            ( min (a |> cellX) (b |> cellX)
            , min (a |> cellY) (b |> cellY)
            )
        , max =
            ( max (a |> cellX) (b |> cellX)
            , max (a |> cellY) (b |> cellY)
            )
        }


maxCell : Viewport -> Cell
maxCell (Viewport vp) =
    vp.max


minCell : Viewport -> Cell
minCell (Viewport vp) =
    vp.min
