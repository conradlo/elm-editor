module Main exposing (Hover(..), InputModifier, KeyboardEventType(..), Model, ModifierKeyType(..), Msg(..), Position, Selection(..), clampColumn, comparePositions, endOfDocument, fontSize, hoversToPositions, init, initModel, insertChar, isEndOfDocument, isFirstColumn, isFirstLine, isLastColumn, isLastLine, isSelected, isStartOfDocument, keyDecoder, keyDownToMsg, keyUpToMsg, lastColumn, lastLine, lineContent, lineHeight, lineLength, main, maxLine, moveDown, moveLeft, moveRight, moveUp, nbsp, newLine, nextLine, onHover, previousLine, removeCharAfter, removeCharBefore, sanitizeHover, selectedText, startOfDocument, subscriptions, update, view, viewChar, viewChars, viewContent, viewCursor, viewDebug, viewEditor, viewLine, viewLineNumber, viewLineNumbers, viewLines, viewSelectedChar)

import Array exposing (Array)
import Browser as Browser exposing (Document)
import Browser.Dom as Dom
import Css as Css exposing (..)
import Css.Global exposing (body, global)
import Html.Styled as H exposing (..)
import Html.Styled.Attributes as H exposing (css, href, src, style)
import Html.Styled.Events as HE exposing (..)
import Json.Decode as JD exposing (Decoder)
import Task


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { lines : Array String
    , cursor : Position
    , hover : Hover
    , selection : Selection
    , modifier : InputModifier
    }


type KeyboardEventType
    = KeyUp
    | KeyDown


type ModifierKeyType
    = Alt
    | Control
    | Meta
    | Shift


type alias InputModifier =
    { alt : Bool
    , control : Bool
    , meta : Bool
    , shift : Bool
    }


type Hover
    = NoHover
    | HoverLine Int
    | HoverChar Position


type Selection
    = NoSelection
    | SelectingFrom Hover
    | SelectedChar Position
    | Selection Position Position


type alias Position =
    { line : Int
    , column : Int
    }


type Msg
    = NoOp
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | NewLine
    | InsertChar Char
    | RemoveCharBefore
    | RemoveCharAfter
    | Hover Hover
    | GoToHoveredPosition
    | StartSelecting
    | StopSelecting
    | ReleaseModifierKey ModifierKeyType
    | HoldModifierKey ModifierKeyType


initModel : Model
initModel =
    { lines = Array.fromList [ "" ]
    , cursor = Position 0 0
    , hover = NoHover
    , selection = NoSelection
    , modifier =
        { control = False
        , meta = False
        , shift = False
        , alt = False
        }
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Dom.focus "editor"
        |> Task.attempt (always NoOp)
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


keyDecoder : KeyboardEventType -> InputModifier -> Decoder ( Msg, Bool )
keyDecoder kind withModifier =
    let
        alwaysPreventDefault : msg -> ( msg, Bool )
        alwaysPreventDefault msg =
            ( msg, True )

        msgDecoder : String -> Decoder Msg
        msgDecoder =
            case kind of
                KeyUp ->
                    keyUpToMsg

                KeyDown ->
                    keyDownToMsg withModifier
    in
    JD.field "key" JD.string
        |> JD.andThen msgDecoder
        |> JD.map alwaysPreventDefault


keyDownToMsg : InputModifier -> String -> Decoder Msg
keyDownToMsg withPrefix string =
    case String.uncons string of
        Just ( char, "" ) ->
            let
                { meta, alt, shift, control } =
                    withPrefix
            in
            if meta || control then
                JD.succeed NoOp

            else
                JD.succeed (InsertChar char)

        _ ->
            case string of
                "ArrowUp" ->
                    JD.succeed MoveUp

                "ArrowDown" ->
                    JD.succeed MoveDown

                "ArrowLeft" ->
                    JD.succeed MoveLeft

                "ArrowRight" ->
                    JD.succeed MoveRight

                "Backspace" ->
                    JD.succeed RemoveCharBefore

                "Delete" ->
                    JD.succeed RemoveCharAfter

                "Enter" ->
                    JD.succeed NewLine

                "Meta" ->
                    JD.succeed (HoldModifierKey Meta)

                "Alt" ->
                    JD.succeed (HoldModifierKey Alt)

                "Shift" ->
                    JD.succeed (HoldModifierKey Shift)

                "Control" ->
                    JD.succeed (HoldModifierKey Control)

                _ ->
                    JD.fail "This key does nothing"


keyUpToMsg : String -> Decoder Msg
keyUpToMsg string =
    case string of
        "Meta" ->
            JD.succeed (ReleaseModifierKey Meta)

        "Alt" ->
            JD.succeed (ReleaseModifierKey Alt)

        "Shift" ->
            JD.succeed (ReleaseModifierKey Shift)

        "Control" ->
            JD.succeed (ReleaseModifierKey Control)

        _ ->
            JD.fail "This key does nothing"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MoveUp ->
            ( { model | cursor = moveUp model.cursor model.lines }
            , Cmd.none
            )

        MoveDown ->
            ( { model | cursor = moveDown model.cursor model.lines }
            , Cmd.none
            )

        MoveLeft ->
            ( { model | cursor = moveLeft model.cursor model.lines }
            , Cmd.none
            )

        MoveRight ->
            ( { model | cursor = moveRight model.cursor model.lines }
            , Cmd.none
            )

        NewLine ->
            ( newLine model
                |> sanitizeHover
            , Cmd.none
            )

        InsertChar char ->
            ( insertChar char model
            , Cmd.none
            )

        RemoveCharBefore ->
            ( removeCharBefore model
                |> sanitizeHover
            , Cmd.none
            )

        RemoveCharAfter ->
            ( removeCharAfter model
                |> sanitizeHover
            , Cmd.none
            )

        Hover hover ->
            ( { model | hover = hover }
                |> sanitizeHover
            , Cmd.none
            )

        GoToHoveredPosition ->
            ( { model
                | cursor =
                    case model.hover of
                        NoHover ->
                            model.cursor

                        HoverLine line ->
                            { line = line
                            , column = lastColumn model.lines line
                            }

                        HoverChar position ->
                            position
              }
            , Cmd.none
            )

        StartSelecting ->
            ( { model | selection = SelectingFrom model.hover }
            , Cmd.none
            )

        StopSelecting ->
            -- Selection for all other
            let
                endHover =
                    model.hover

                newSelection =
                    case model.selection of
                        NoSelection ->
                            NoSelection

                        SelectingFrom startHover ->
                            if startHover == endHover then
                                case startHover of
                                    NoHover ->
                                        NoSelection

                                    HoverLine _ ->
                                        NoSelection

                                    HoverChar position ->
                                        SelectedChar position

                            else
                                hoversToPositions model.lines startHover endHover
                                    |> Maybe.map (\( from, to ) -> Selection from to)
                                    |> Maybe.withDefault NoSelection

                        SelectedChar _ ->
                            NoSelection

                        Selection _ _ ->
                            NoSelection
            in
            ( { model | selection = newSelection }
            , Cmd.none
            )

        HoldModifierKey key ->
            let
                previous =
                    model.modifier
            in
            case key of
                Meta ->
                    ( { model | modifier = { previous | meta = True } }, Cmd.none )

                Alt ->
                    ( { model | modifier = { previous | alt = True } }, Cmd.none )

                Shift ->
                    ( { model | modifier = { previous | shift = True } }, Cmd.none )

                Control ->
                    ( { model | modifier = { previous | control = True } }, Cmd.none )

        ReleaseModifierKey key ->
            let
                previous =
                    model.modifier
            in
            case key of
                Meta ->
                    ( { model | modifier = { previous | meta = False } }, Cmd.none )

                Alt ->
                    ( { model | modifier = { previous | alt = False } }, Cmd.none )

                Shift ->
                    ( { model | modifier = { previous | shift = False } }, Cmd.none )

                Control ->
                    ( { model | modifier = { previous | control = False } }, Cmd.none )


hoversToPositions : Array String -> Hover -> Hover -> Maybe ( Position, Position )
hoversToPositions lines from to =
    let
        selectionLinePosition : Int -> Position -> ( Position, Position )
        selectionLinePosition line position =
            if line >= position.line then
                ( position
                , { line = line, column = lastColumn lines line }
                )

            else
                ( { line = line + 1, column = 0 }
                , position
                )
    in
    case ( from, to ) of
        ( NoHover, _ ) ->
            Nothing

        ( _, NoHover ) ->
            Nothing

        ( HoverLine line1, HoverLine line2 ) ->
            let
                smaller =
                    min line1 line2

                bigger =
                    max line1 line2
            in
            Just
                ( { line = smaller + 1, column = 0 }
                , { line = bigger, column = lastColumn lines bigger }
                )

        ( HoverLine line, HoverChar position ) ->
            Just (selectionLinePosition line position)

        ( HoverChar position, HoverLine line ) ->
            Just (selectionLinePosition line position)

        ( HoverChar position1, HoverChar position2 ) ->
            let
                ( smaller, bigger ) =
                    if comparePositions position1 position2 == LT then
                        ( position1, position2 )

                    else
                        ( position2, position1 )
            in
            Just ( smaller, bigger )


comparePositions : Position -> Position -> Order
comparePositions from to =
    if from.line < to.line || (from.line == to.line && from.column < to.column) then
        LT

    else if from == to then
        EQ

    else
        GT


sanitizeHover : Model -> Model
sanitizeHover model =
    { model
        | hover =
            case model.hover of
                NoHover ->
                    model.hover

                HoverLine line ->
                    HoverLine (clamp 0 (lastLine model.lines) line)

                HoverChar { line, column } ->
                    let
                        sanitizedLine =
                            clamp 0 (lastLine model.lines) line

                        sanitizedColumn =
                            clamp 0 (lastColumn model.lines sanitizedLine) column
                    in
                    HoverChar
                        { line = sanitizedLine
                        , column = sanitizedColumn
                        }
    }


newLine : Model -> Model
newLine ({ cursor, lines } as model) =
    let
        { line, column } =
            cursor

        linesList : List String
        linesList =
            Array.toList lines

        line_ : Int
        line_ =
            line + 1

        contentUntilCursor : List String
        contentUntilCursor =
            linesList
                |> List.take line_
                |> List.indexedMap
                    (\i content ->
                        if i == line then
                            String.left column content

                        else
                            content
                    )

        restOfLineAfterCursor : String
        restOfLineAfterCursor =
            String.dropLeft column (lineContent lines line)

        restOfLines : List String
        restOfLines =
            List.drop line_ linesList

        newLines : Array String
        newLines =
            (contentUntilCursor
                ++ [ restOfLineAfterCursor ]
                ++ restOfLines
            )
                |> Array.fromList

        newCursor : Position
        newCursor =
            { line = line_
            , column = 0
            }
    in
    { model
        | lines = newLines
        , cursor = newCursor
    }


insertChar : Char -> Model -> Model
insertChar char ({ cursor, lines } as model) =
    let
        { line, column } =
            cursor

        lineWithCharAdded : String -> String
        lineWithCharAdded content =
            String.left column content
                ++ String.fromChar char
                ++ String.dropLeft column content

        newLines : Array String
        newLines =
            lines
                |> Array.indexedMap
                    (\i content ->
                        if i == line then
                            lineWithCharAdded content

                        else
                            content
                    )

        newCursor : Position
        newCursor =
            { line = line
            , column = column + 1
            }
    in
    { model
        | lines = newLines
        , cursor = newCursor
    }


removeCharBefore : Model -> Model
removeCharBefore ({ cursor, lines } as model) =
    if isStartOfDocument cursor then
        model

    else
        let
            { line, column } =
                cursor

            lineIsEmpty : Bool
            lineIsEmpty =
                lineContent lines line
                    |> String.isEmpty

            removeCharFromLine : ( Int, String ) -> List String
            removeCharFromLine ( lineNum, content ) =
                if lineNum == line - 1 then
                    if isFirstColumn column then
                        [ content ++ lineContent lines line ]

                    else
                        [ content ]

                else if lineNum == line then
                    if isFirstColumn column then
                        []

                    else
                        [ String.left (column - 1) content
                            ++ String.dropLeft column content
                        ]

                else
                    [ content ]

            newLines : Array String
            newLines =
                lines
                    |> Array.toIndexedList
                    |> List.concatMap removeCharFromLine
                    |> Array.fromList
        in
        { model
            | lines = newLines
            , cursor = moveLeft cursor lines
        }


removeCharAfter : Model -> Model
removeCharAfter ({ cursor, lines } as model) =
    if isEndOfDocument lines cursor then
        model

    else
        let
            { line, column } =
                cursor

            isOnLastColumn : Bool
            isOnLastColumn =
                isLastColumn lines line column

            removeCharFromLine : ( Int, String ) -> List String
            removeCharFromLine ( lineNum, content ) =
                if lineNum == line then
                    if isOnLastColumn then
                        [ content ++ lineContent lines (line + 1) ]

                    else
                        [ String.left column content
                            ++ String.dropLeft (column + 1) content
                        ]

                else if lineNum == line + 1 then
                    if isOnLastColumn then
                        []

                    else
                        [ content ]

                else
                    [ content ]

            newLines : Array String
            newLines =
                lines
                    |> Array.toIndexedList
                    |> List.concatMap removeCharFromLine
                    |> Array.fromList
        in
        { model
            | lines = newLines
            , cursor = cursor
        }


moveUp : Position -> Array String -> Position
moveUp { line, column } lines =
    if isFirstLine line then
        startOfDocument

    else
        let
            line_ : Int
            line_ =
                previousLine line
        in
        { line = line_
        , column = clampColumn lines line_ column
        }


moveDown : Position -> Array String -> Position
moveDown { line, column } lines =
    if isLastLine lines line then
        endOfDocument lines

    else
        let
            line_ : Int
            line_ =
                nextLine lines line
        in
        { line = line_
        , column = clampColumn lines line_ column
        }


moveLeft : Position -> Array String -> Position
moveLeft ({ line, column } as position) lines =
    if isStartOfDocument position then
        position

    else if isFirstColumn column then
        let
            line_ : Int
            line_ =
                previousLine line
        in
        { line = line_
        , column = lastColumn lines line_
        }

    else
        { line = line
        , column = column - 1
        }


moveRight : Position -> Array String -> Position
moveRight ({ line, column } as position) lines =
    if isEndOfDocument lines position then
        position

    else if isLastColumn lines line column then
        { line = nextLine lines line
        , column = 0
        }

    else
        { line = line
        , column = column + 1
        }


startOfDocument : Position
startOfDocument =
    { line = 0
    , column = 0
    }


endOfDocument : Array String -> Position
endOfDocument lines =
    { line = lastLine lines
    , column = lastColumn lines (lastLine lines)
    }


isStartOfDocument : Position -> Bool
isStartOfDocument { line, column } =
    isFirstLine line
        && isFirstColumn column


isEndOfDocument : Array String -> Position -> Bool
isEndOfDocument lines { line, column } =
    isLastLine lines line
        && isLastColumn lines line column


isFirstLine : Int -> Bool
isFirstLine line =
    line == 0


isLastLine : Array String -> Int -> Bool
isLastLine lines line =
    line == lastLine lines


isFirstColumn : Int -> Bool
isFirstColumn column =
    column == 0


isLastColumn : Array String -> Int -> Int -> Bool
isLastColumn lines line column =
    column == lastColumn lines line


lastLine : Array String -> Int
lastLine lines =
    Array.length lines - 1


previousLine : Int -> Int
previousLine line =
    (line - 1)
        |> max 0


nextLine : Array String -> Int -> Int
nextLine lines line =
    (line + 1)
        |> min (maxLine lines)


maxLine : Array String -> Int
maxLine lines =
    Array.length lines - 1


lastColumn : Array String -> Int -> Int
lastColumn lines line =
    lineLength lines line


clampColumn : Array String -> Int -> Int -> Int
clampColumn lines line column =
    column
        |> clamp 0 (lineLength lines line)


lineContent : Array String -> Int -> String
lineContent lines lineNum =
    lines
        |> Array.get lineNum
        |> Maybe.withDefault ""


lineLength : Array String -> Int -> Int
lineLength lines lineNum =
    lineContent lines lineNum
        |> String.length


testDiv : Html Msg
testDiv =
    H.div [] []


view : Model -> Document Msg
view model =
    let
        app =
            H.div []
                [ global
                    [ body
                        [ Css.padding (pct 0)
                        , Css.margin (pct 0)
                        ]
                    ]
                , viewEditor model
                , viewDebug model
                ]
    in
    { title = "elm-editor"
    , body = [ H.toUnstyled app ]
    }


viewDebug : Model -> Html Msg
viewDebug { lines, cursor, hover, selection, modifier } =
    let
        printPosition : Position -> String
        printPosition pos =
            "line: " ++ String.fromInt cursor.line ++ " column: " ++ String.fromInt cursor.column

        printHoverState : Hover -> String
        printHoverState state =
            case state of
                NoHover ->
                    "NoHover"

                HoverLine lineNo ->
                    "HoverLine " ++ String.fromInt lineNo

                HoverChar pos ->
                    "HoverChar " ++ printPosition pos

        printSelection : Selection -> String
        printSelection state =
            case state of
                NoSelection ->
                    "NoSelection"

                SelectingFrom h ->
                    "SelectingFrom " ++ printHoverState h

                SelectedChar position ->
                    "SelectedChar " ++ printPosition position

                Selection startPos endPos ->
                    "Selection " ++ printPosition startPos ++ " " ++ printPosition endPos

        printLines : Array String -> List (Html msg)
        printLines lns =
            let
                reducer : String -> List (Html msg) -> List (Html msg)
                reducer str result =
                    result ++ [ H.text str, H.br [] [] ]
            in
            List.foldl reducer [] (Array.toList lns)

        printSuperKeys : InputModifier -> String
        printSuperKeys { control, meta, alt, shift } =
            let
                convert label keyTypeState =
                    if keyTypeState then
                        label

                    else
                        ""
            in
            [ convert "control" control
            , convert "meta" meta
            , convert "alt" alt
            , convert "shift" shift
            ]
                |> List.filter (\s -> s /= "")
                |> String.join " "
    in
    H.div
        [ css
            [ maxWidth (pct 100.0)
            ]
        ]
        [ H.text "lines:"
        , H.pre
            [ css [ whiteSpace preWrap ]
            ]
            (printLines lines)
        , H.text "cursor:"
        , H.pre [] [ H.text (printPosition cursor) ]
        , H.text "hover:"
        , H.pre [] [ H.text (printHoverState hover) ]
        , H.text "selection:"
        , H.pre [] [ H.text (printSelection selection) ]
        , H.text "selected text:"
        , H.pre [] [ H.text (selectedText selection hover lines) ]
        , H.text "super keys:"
        , H.pre [] [ H.text (printSuperKeys modifier) ]
        ]


selectedText : Selection -> Hover -> Array String -> String
selectedText selection currentHover lines =
    let
        positionsToString : Position -> Position -> String
        positionsToString from to =
            let
                numberOfLines =
                    to.line - from.line + 1
            in
            lines
                |> Array.toList
                |> List.drop from.line
                |> List.take numberOfLines
                |> List.indexedMap
                    (\i line ->
                        if numberOfLines == 1 then
                            line
                                |> String.dropLeft from.column
                                |> String.left (to.column - from.column + 1)

                        else if i == 0 then
                            String.dropLeft from.column line

                        else if i == numberOfLines - 1 then
                            String.left (to.column + 1) line

                        else
                            line
                    )
                |> String.join "\n"
    in
    case selection of
        NoSelection ->
            ""

        SelectingFrom startHover ->
            hoversToPositions lines startHover currentHover
                |> Maybe.map (\( from, to ) -> positionsToString from to)
                |> Maybe.withDefault ""

        SelectedChar { line, column } ->
            lineContent lines line
                |> String.dropLeft column
                |> String.left 1

        Selection from to ->
            positionsToString from to


viewEditor : Model -> Html Msg
viewEditor model =
    let
        { modifier } =
            model
    in
    H.div
        [ css
            [ displayFlex
            , height (vh 100)
            , flexDirection row
            , fontFamily monospace
            , Css.fontSize (px fontSize)
            , Css.lineHeight (px lineHeight)
            , whiteSpace Css.pre
            ]
        , H.tabindex 0
        , H.id "editor"
        , HE.preventDefaultOn "keydown" (keyDecoder KeyDown modifier)
        , HE.preventDefaultOn "keyup" (keyDecoder KeyUp modifier)
        ]
        [ viewLineNumbers model
        , viewContent model
        ]


viewLineNumbers : Model -> Html Msg
viewLineNumbers model =
    H.div
        [ css
            [ width (Css.em 2)
            , textAlign center
            , color (hex "#888")
            , displayFlex
            , flexDirection column
            ]
        ]
        (List.range 1 (Array.length model.lines)
            |> List.map viewLineNumber
        )


viewLineNumber : Int -> Html Msg
viewLineNumber n =
    H.span [] [ H.text (String.fromInt n) ]


viewContent : Model -> Html Msg
viewContent model =
    H.div
        [ css
            [ position relative
            , flex (int 1)
            , backgroundColor (hex "#f0f0f0")
            ]
        , style "user-select" "none"
        , HE.onMouseDown StartSelecting
        , HE.onMouseUp StopSelecting
        , HE.onClick GoToHoveredPosition
        , HE.onMouseOut (Hover NoHover)
        ]
        [ viewLines model.cursor model.hover model.selection model.lines ]


viewLines : Position -> Hover -> Selection -> Array String -> Html Msg
viewLines position hover selection lines =
    H.div []
        (lines
            |> Array.indexedMap (viewLine position hover selection lines)
            |> Array.toList
        )


viewLine : Position -> Hover -> Selection -> Array String -> Int -> String -> Html Msg
viewLine position hover selection lines line content =
    H.div
        [ css
            [ Css.position absolute
            , left (px 0)
            , right (px 0)
            , height (px lineHeight)
            , top (px (toFloat line * lineHeight))
            ]
        , HE.onMouseOver (Hover (HoverLine line))
        ]
        (if position.line == line && isLastColumn lines line position.column then
            viewChars position hover selection lines line content
                ++ [ viewCursor position nbsp ]

         else
            viewChars position hover selection lines line content
        )


viewChars : Position -> Hover -> Selection -> Array String -> Int -> String -> List (Html Msg)
viewChars position hover selection lines line content =
    content
        |> String.toList
        |> List.indexedMap (viewChar position hover selection lines line)


viewChar : Position -> Hover -> Selection -> Array String -> Int -> Int -> Char -> Html Msg
viewChar position hover selection lines line column char =
    if position.line == line && position.column == column then
        viewCursor
            position
            (String.fromChar char)

    else if selection /= NoSelection && isSelected lines selection hover line column then
        viewSelectedChar
            { line = line, column = column }
            (String.fromChar char)

    else
        H.span
            [ onHover { line = line, column = column } ]
            [ H.text (String.fromChar char) ]


isSelected : Array String -> Selection -> Hover -> Int -> Int -> Bool
isSelected lines selection currentHover line column =
    let
        isSelectedPositions : Position -> Position -> Bool
        isSelectedPositions from to =
            (from.line <= line)
                && (to.line >= line)
                && (if from.line == line then
                        from.column <= column

                    else
                        True
                   )
                && (if to.line == line then
                        to.column >= column

                    else
                        True
                   )
    in
    case selection of
        NoSelection ->
            False

        SelectingFrom startHover ->
            hoversToPositions lines startHover currentHover
                |> Maybe.map (\( from, to ) -> isSelectedPositions from to)
                |> Maybe.withDefault False

        SelectedChar position ->
            position == { line = line, column = column }

        Selection from to ->
            isSelectedPositions from to


nbsp : String
nbsp =
    "\u{00A0}"


viewCursor : Position -> String -> Html Msg
viewCursor position char =
    H.span
        [ css [ backgroundColor (hex "#471437") ]
        , onHover position
        ]
        [ H.text char ]


viewSelectedChar : Position -> String -> Html Msg
viewSelectedChar position char =
    H.span
        [ css [ "#ccc" |> hex |> backgroundColor ]
        , onHover position
        ]
        [ H.text char ]


onHover : Position -> Attribute Msg
onHover position =
    HE.custom "mouseover"
        (JD.succeed
            { message = HoverChar position |> Hover
            , stopPropagation = True
            , preventDefault = True
            }
        )


fontSize : Float
fontSize =
    20


lineHeight : Float
lineHeight =
    fontSize * 1.2
