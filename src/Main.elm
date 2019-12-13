port module Main exposing (main)

import Array exposing (Array)
import Array.Extra as Array
import Browser
import Browser.Dom
import Browser.Events
import Csv
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import Html.Lazy exposing (lazy3, lazy4)
import Json.Decode as JD
import Json.Encode as JE
import Task



-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias CellRef =
    { row : Int
    , col : Int
    }


type Cell
    = Float Float
    | String String


cellToString : Cell -> String
cellToString cell =
    case cell of
        Float float ->
            String.fromFloat float

        String str ->
            str


type alias Table =
    Array (Array Cell)


getTableSize : Table -> CellRef
getTableSize table =
    case Array.get 0 table of
        Just firstRow ->
            CellRef (Array.length table) (Array.length firstRow)

        Nothing ->
            CellRef (Array.length table) 0


toCsv : Table -> String
toCsv table =
    let
        escapeDQ : Cell -> String
        escapeDQ cell =
            if String.contains "\"" <| cellToString cell then
                "\"" ++ (String.replace "\"" "\"\"" <| cellToString cell) ++ "\""

            else
                cellToString cell

        listTable =
            Array.toList <| Array.map (Array.toList << Array.map escapeDQ) table
    in
    String.join "\n" <| List.map (String.join ",") listTable


fromCsv : String -> Table
fromCsv csvStr =
    let
        { headers, records } =
            Csv.parse csvStr
    in
    Array.fromList <|
        [ Array.fromList <| List.map String headers ]
            ++ List.map (Array.fromList << List.map String) records


type Select
    = Cell CellRef
    | Csv


type alias InputState =
    { selected : Select
    , isEditing : Bool
    , text : String
    }


type alias Model =
    { table : Table
    , input : InputState
    }


init : JE.Value -> ( Model, Cmd Msg )
init data =
    let
        initTable =
            Array.fromList <| List.map Array.fromList [ [ String "Name", String "Age" ], [ String "Bob", Float 18 ] ]

        initInput =
            InputState (Cell <| CellRef 0 0) False ""
    in
    case JD.decodeValue (JD.array (JD.array jsonToCell)) data of
        Ok table ->
            ( Model table initInput, Cmd.none )

        Err _ ->
            ( Model initTable initInput, Cmd.none )


jsonToCell : JD.Decoder Cell
jsonToCell =
    JD.field "type" JD.string
        |> JD.andThen toCell


toCell : String -> JD.Decoder Cell
toCell typeStr =
    case typeStr of
        "Float" ->
            JD.map Float (JD.field "Float" JD.float)

        "String" ->
            JD.map String (JD.field "String" JD.string)

        _ ->
            JD.fail "operation JSON to Cell failed"



-- PORT


port saveTable : JE.Value -> Cmd msg


tableToJson : Table -> JE.Value
tableToJson table =
    JE.array (JE.array cellToJson) table


cellToJson : Cell -> JE.Value
cellToJson cell =
    case cell of
        Float float ->
            JE.object [ ( "type", JE.string "Float" ), ( "Float", JE.float float ) ]

        String str ->
            JE.object [ ( "type", JE.string "String" ), ( "String", JE.string str ) ]



-- UPDATE


type Move
    = Up
    | Down
    | Left
    | Right


moveCellRef : Move -> CellRef -> Table -> CellRef
moveCellRef move cellRef table =
    let
        { row, col } =
            cellRef

        size =
            getTableSize table
    in
    case move of
        Up ->
            { cellRef | row = max 0 (row - 1) }

        Down ->
            { cellRef | row = min (size.row - 1) (row + 1) }

        Left ->
            { cellRef | col = max 0 (col - 1) }

        Right ->
            { cellRef | col = min (size.col - 1) (col + 1) }


type Msg
    = Select Select
    | Move Move
    | StartEdit (Maybe CellRef) String
    | CancelEdit
    | Edit String
    | EditCsv String
    | Set
    | AddRow Int
    | AddCol Int
    | SaveToJs
    | None


focusCellEditor : Cmd Msg
focusCellEditor =
    Task.attempt (\_ -> None) (Browser.Dom.focus "cell-input")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select select ->
            case select of
                Cell cellRef ->
                    ( { model | input = InputState (Cell cellRef) False "" }, Cmd.none )

                Csv ->
                    ( { model | input = InputState Csv True "" }, Cmd.none )

        Move move ->
            case model.input.selected of
                Cell cellRef ->
                    let
                        oldInput =
                            model.input

                        newInput =
                            { oldInput | selected = Cell <| moveCellRef move cellRef model.table }
                    in
                    ( { model | input = newInput }, Cmd.none )

                Csv ->
                    ( model, Cmd.none )

        StartEdit (Just cellRef) text ->
            ( { model | input = InputState (Cell cellRef) True text }, focusCellEditor )

        StartEdit Nothing text ->
            ( { model | input = InputState model.input.selected True text }, focusCellEditor )

        CancelEdit ->
            let
                prevInput =
                    model.input

                newInput =
                    { prevInput | isEditing = False, text = "" }
            in
            ( { model | input = newInput }, Cmd.none )

        Edit text ->
            let
                prevInput =
                    model.input

                newInput =
                    { prevInput | text = text }
            in
            ( { model | input = newInput }, Cmd.none )

        EditCsv csvStr ->
            update SaveToJs
                { model | table = fromCsv csvStr }

        Set ->
            case model.input.selected of
                Cell cellRef ->
                    case String.toFloat model.input.text of
                        Just num ->
                            update SaveToJs
                                { model
                                    | table = updateData cellRef (Float num) model.table
                                    , input = InputState (Cell cellRef) False ""
                                }

                        Nothing ->
                            update SaveToJs
                                { model
                                    | table = updateData cellRef (String model.input.text) model.table
                                    , input = InputState (Cell cellRef) False ""
                                }

                Csv ->
                    ( model, Cmd.none )

        AddRow rowIndex ->
            let
                ( first, second ) =
                    Array.splitAt rowIndex model.table

                { col } =
                    getTableSize model.table

                newTable =
                    Array.append (Array.push (Array.repeat col <| String "") first) second
            in
            update SaveToJs
                { model | table = newTable }

        AddCol colIndex ->
            let
                mapFunc : Array Cell -> Array Cell
                mapFunc row =
                    Array.append (Array.push (String "") <| Array.sliceUntil colIndex row) (Array.sliceFrom colIndex row)
            in
            update SaveToJs
                { model | table = Array.map mapFunc model.table }

        SaveToJs ->
            ( model, saveTable <| tableToJson <| model.table )

        None ->
            ( model, Cmd.none )


updateData : CellRef -> Cell -> Table -> Table
updateData { row, col } data table =
    case Array.get row table of
        Just rowArray ->
            Array.set row (Array.set col data rowArray) table

        Nothing ->
            table



-- SUBSCRIPTIONS


keyDecoderDown : JD.Decoder Msg
keyDecoderDown =
    JD.map toDirection (JD.field "key" JD.string)


toDirection : String -> Msg
toDirection string =
    case String.length string of
        1 ->
            StartEdit Nothing string

        _ ->
            case string of
                "ArrowLeft" ->
                    Move Left

                "ArrowUp" ->
                    Move Up

                "ArrowRight" ->
                    Move Right

                "ArrowDown" ->
                    Move Down

                "Escape" ->
                    CancelEdit

                "Enter" ->
                    StartEdit Nothing ""

                _ ->
                    None


subscriptions : Model -> Sub Msg
subscriptions { input } =
    if input.isEditing then
        Sub.none

    else
        Browser.Events.onKeyDown keyDecoderDown



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Excel"
    , body = [ viewTable model, viewTemp ]
    }


viewTemp : Html Msg
viewTemp =
    div [] [ button [ E.onClick <| AddCol 1 ] [ text "Add col" ] ]


viewTable : Model -> Html Msg
viewTable model =
    table []
        [ tbody [] (List.indexedMap (lazy3 viewRow model.input) (Array.toList model.table))
        ]


viewRow : InputState -> Int -> Array Cell -> Html Msg
viewRow inputState row data =
    tr []
        (List.indexedMap (lazy4 viewCell inputState row) (Array.toList data))


viewCell : InputState -> Int -> Int -> Cell -> Html Msg
viewCell inputState row col cell =
    let
        cellRef =
            CellRef row col

        isSelected =
            inputState.selected == Cell cellRef

        selectedClass =
            if isSelected then
                "selected"

            else
                ""
    in
    if inputState.isEditing && isSelected then
        td [ A.class "selected" ]
            [ form [ E.onSubmit Set ]
                [ input
                    [ A.id "cell-input"
                    , A.value inputState.text
                    , E.onInput Edit
                    , onEscapeKey
                    ]
                    []
                ]
            ]

    else
        td
            [ A.class selectedClass
            , E.onClick (Select <| Cell cellRef)
            , E.onDoubleClick (StartEdit (Just cellRef) <| cellToString cell)
            ]
            [ text <| cellToString cell ]


onEscapeKey : Attribute Msg
onEscapeKey =
    let
        func : String -> Msg
        func key =
            if key == "Escape" then
                CancelEdit

            else
                None
    in
    E.on "keydown" (JD.map func (JD.field "key" JD.string))
