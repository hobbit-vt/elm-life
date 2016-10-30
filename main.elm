import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.App as App
import Html.Events exposing (onClick)
import Array exposing (Array)
import Random exposing (Generator)
import Debug exposing (..)
import Time exposing (Time)

size: Int
size = 25

main : Program Never
main =
  App.program
    { init = (Array.empty, genRow(size))
    , view = view 
    , update = update
    , subscriptions = \_ -> Time.every Time.second (\_ -> Turn)
    }

type Cell = LivedCell | FreeCell
type alias Row = Array Cell
type alias Field = Array Row
type alias Model = Field

type Msg
  = RowGenerated Row
  | Turn

update : Msg -> Model -> (Model, Cmd Msg)
update msg field =
  case msg of
    RowGenerated row ->
      if Array.length field < size - 1 then
        (Array.push row field) ! [ genRow(size) ] 
      else
        (Array.push row field) ! []
    Turn ->
      doTurn field ! []

view : Model -> Html Msg
view field =
  div [] (
    field
      |> Array.map viewRow
      |> Array.toList
  )
  

viewRow : Row -> Html msg
viewRow row =
  div [] (
    row
      |> Array.map viewCell
      |> Array.toList
  )

viewCell : Cell -> Html msg
viewCell cell =
  let
    cellParams = case cell of
        LivedCell -> { text = "X", color = "green" }
        FreeCell -> { text = "O", color = "red" }
  in
    div [
      style [ ("display", "inline-block"), ("width", "25px"), ("height", "25px"), ("background", cellParams.color) ]
    ] [ 
      text cellParams.text
    ]


genRow : Int -> Cmd Msg
genRow size =
  Random.list size Random.bool
    |> Random.map bools2cells
    |> Random.generate RowGenerated

bools2cells: List Bool -> Array Cell
bools2cells bs = List.map bool2cell bs |> Array.fromList 

bool2cell: Bool -> Cell
bool2cell b = if b then LivedCell else FreeCell


doTurn : Field -> Field
doTurn field = snd (processTurn (field, field) (0, 0))

processTurn : (Field, Field) -> (Int, Int) -> (Field, Field)
processTurn (curField, newField) (x, y) = 
  if y == size then
    (curField, newField)
  else if x == size then
    processTurn (curField, newField) (0, y+1)
  else
    let
      livedCellCount =
        computeSiblings (x, y)
          |> List.map (retrieveCell curField)
          |> List.filter (\cell -> cell == LivedCell)
          |> List.length
      curCell = retrieveCell curField (x, y)
      newCell = resolveCell livedCellCount curCell   
    in
      processTurn
        (curField, (setCell newField (x, y) newCell))
        (x+1, y)


retrieveCell : Field -> (Int, Int) -> Cell
retrieveCell field (rawX, rawY) =
  let
    x = normalizeFieldIndex rawX
    y = normalizeFieldIndex rawY
  in
    Array.get y field
      |> flatMap (Array.get x)
      |> Maybe.withDefault (FreeCell) -- IMPOSSIBLE!!! 

flatMap: (a -> Maybe b) -> Maybe a -> Maybe b
flatMap fn mb =
  Maybe.andThen mb fn

normalizeFieldIndex : Int -> Int
normalizeFieldIndex index =
  if index == -1 then
    size-1
  else if index == size then
    0
  else
    index

computeSiblings : (Int, Int) -> List (Int, Int)
computeSiblings (x, y) =
  [ (x-1, y+1), (x, y+1), (x+1, y+1), 
    (x-1, y)            , (x+1, y),
    (x-1, y-1), (x, y-1), (x+1, y-1) ]

resolveCell : Int -> Cell -> Cell
resolveCell livedCellCount cell = 
  case cell of
    LivedCell -> 
      if livedCellCount == 2 || livedCellCount == 3 then
        LivedCell
      else
        FreeCell 
    FreeCell ->
      if livedCellCount == 3 then
        LivedCell
      else
        FreeCell

setCell : Field -> (Int, Int) -> Cell -> Field
setCell field (x, y) cell =
  case (Array.get y field) of
    Just row ->  
      Array.set y (Array.set x cell row) field
    Nothing -> Array.empty



