module Tests.RemoveCharAfter exposing (..)

import Array
import ArchitectureTest exposing (..)
import Expect exposing (Expectation)
import Main exposing (isEndOfDocument, isLastColumn, isLastLine, lineContent, lineLength)
import Test exposing (..)
import Tests.Common exposing (..)


doesNothingOnEndOfDocument : Test
doesNothingOnEndOfDocument =
    msgTestWithPrecondition "RemoveCharAfter does nothing on start of document"
        app
        removeCharAfter
        (\model -> isEndOfDocument model.lines model.cursor)
    <|
        \modelBeforeMsg msg finalModel ->
            finalModel
                |> Expect.equal modelBeforeMsg


doesntMove : Test
doesntMove =
    msgTest "RemoveCharAfter doesn't move"
        app
        removeCharAfter
    <|
        \modelBeforeMsg msg finalModel ->
            finalModel.cursor
                |> Expect.equal modelBeforeMsg.cursor


decreasesLineCountOnLastColumnOfNotLastLine : Test
decreasesLineCountOnLastColumnOfNotLastLine =
    msgTestWithPrecondition "RemoveCharAfter decreases line count on last column of not last line"
        app
        removeCharAfter
        (\model ->
            isLastColumn model.lines model.cursor.line model.cursor.column
                && not (isLastLine model.lines model.cursor.line)
        )
    <|
        \modelBeforeMsg msg finalModel ->
            Array.length finalModel.lines
                |> Expect.equal (Array.length modelBeforeMsg.lines - 1)


combinesLinesTogetherOnLastColumnOfNotLastLine : Test
combinesLinesTogetherOnLastColumnOfNotLastLine =
    msgTestWithPrecondition "RemoveCharAfter combines lines together on last column of not last line"
        app
        removeCharAfter
        (\model ->
            isLastColumn model.lines model.cursor.line model.cursor.column
                && not (isLastLine model.lines model.cursor.line)
        )
    <|
        \modelBeforeMsg msg finalModel ->
            let
                oldLine1 =
                    lineContent modelBeforeMsg.lines modelBeforeMsg.cursor.line

                oldLine2 =
                    lineContent modelBeforeMsg.lines (modelBeforeMsg.cursor.line + 1)

                expectedNewLine =
                    oldLine1 ++ oldLine2

                newLine =
                    lineContent finalModel.lines finalModel.cursor.line
            in
            newLine
                |> Expect.equal expectedNewLine


decreasesCurrentLineLengthOnNotLastColumn : Test
decreasesCurrentLineLengthOnNotLastColumn =
    msgTestWithPrecondition "RemoveCharAfter decreases current line length on not last column"
        app
        removeCharAfter
        (\model -> not (isLastColumn model.lines model.cursor.line model.cursor.column))
    <|
        \modelBeforeMsg msg finalModel ->
            let
                newLineLength =
                    lineLength finalModel.lines finalModel.cursor.line

                oldLineLength =
                    lineLength modelBeforeMsg.lines modelBeforeMsg.cursor.line
            in
            newLineLength
                |> Expect.equal (oldLineLength - 1)


removesTheCharOnNotLastColumn : Test
removesTheCharOnNotLastColumn =
    msgTestWithPrecondition "RemoveCharAfter removes the char on not last column"
        app
        removeCharAfter
        (\model -> not (isLastColumn model.lines model.cursor.line model.cursor.column))
    <|
        \modelBeforeMsg msg finalModel ->
            let
                oldLine =
                    lineContent modelBeforeMsg.lines modelBeforeMsg.cursor.line

                oldLineBefore =
                    oldLine
                        |> String.left modelBeforeMsg.cursor.column

                oldLineAfter =
                    oldLine
                        |> String.dropLeft (modelBeforeMsg.cursor.column + 1)

                expectedNewLine =
                    oldLineBefore ++ oldLineAfter

                newLine =
                    lineContent finalModel.lines finalModel.cursor.line
            in
            newLine
                |> Expect.equal expectedNewLine
