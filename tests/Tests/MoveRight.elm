module Tests.MoveRight exposing (..)

import ArchitectureTest exposing (..)
import Expect exposing (Expectation)
import Main exposing (isEndOfDocument, lastColumn, lastLine)
import Test exposing (..)
import Tests.Common exposing (..)


doesNothingOnEndOfDocument : Test
doesNothingOnEndOfDocument =
    msgTestWithPrecondition "MoveRight does nothing on end of document"
        app
        moveRight
        (\model -> isEndOfDocument model.lines model.cursor)
    <|
        \modelBeforeMsg _ finalModel ->
            finalModel
                |> Expect.equal modelBeforeMsg


movesRightIfNotOnLastColumn : Test
movesRightIfNotOnLastColumn =
    msgTestWithPrecondition "MoveRight moves right if not on last column"
        app
        moveRight
        (\model -> model.cursor.column /= lastColumn model.lines model.cursor.line)
    <|
        \modelBeforeMsg _ finalModel ->
            finalModel.cursor.column
                |> Expect.equal (modelBeforeMsg.cursor.column + 1)


movesToStartOfNextLineIfOnTheLastColumnAndNotOnLastLine : Test
movesToStartOfNextLineIfOnTheLastColumnAndNotOnLastLine =
    msgTestWithPrecondition "MoveRight moves to start of next line if on the last column and not on last line"
        app
        moveRight
        (\model ->
            (model.cursor.column == lastColumn model.lines model.cursor.line)
                && (model.cursor.line /= lastLine model.lines)
        )
    <|
        \modelBeforeMsg _ finalModel ->
            Expect.all
                [ .line >> Expect.equal (modelBeforeMsg.cursor.line + 1)
                , .column >> Expect.equal 0
                ]
                finalModel.cursor
