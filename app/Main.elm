module Main exposing (..)

import Html exposing (Html, text, input, code, div)
import Set exposing (Set)


main =
    Html.program
        { init = ( defaults, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { defaultCell : CellState
    , flippedCells : Set ( Int, Int )
    , cellsInView : Viewport
    }


type CellState
    = Empty
    | Filled


type alias Viewport =
    { xmin : Int
    , xmax : Int
    , ymin : Int
    , ymax : Int
    }


defaults : Model
defaults =
    { defaultCell = Empty
    , flippedCells = Set.fromList [ ( 0, 0 ) ]
    , cellsInView = { xmin = -8, xmax = 8, ymin = -8, ymax = 8 }
    }


type Msg
    = NoAction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ text "wut" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
