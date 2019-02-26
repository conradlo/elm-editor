module Tests.InsertChar exposing (..)

import ArchitectureTest exposing (..)
import Expect exposing (Expectation)
import Main exposing (Msg(..), lineContent, lineLength)
import Test exposing (..)
import Tests.Common exposing (..)


movesRight : Test
movesRight =
    msgTest "InsertChar moves right"
        app
        insertChar
    <|
        (\modelBeforeMsg msg finalModel   ->
            finalModel.cursor.column
                |> Expect.equal (modelBeforeMsg.cursor.column + 1))


doesntMoveUpOrDown : Test
doesntMoveUpOrDown =
    msgTest "InsertChar doesn't move up or down"
        app
        insertChar
    <|
        \modelBeforeMsg msg finalModel ->
            finalModel.cursor.line
                |> Expect.equal modelBeforeMsg.cursor.line


makesLineLonger : Test
makesLineLonger =
    msgTest "InsertChar makes line longer"
        app
        insertChar
    <|
        \modelBeforeMsg msg finalModel ->
            lineLength finalModel.lines finalModel.cursor.line
                |> Expect.equal (lineLength modelBeforeMsg.lines modelBeforeMsg.cursor.line + 1)


insertsChar : Test
insertsChar =
    msgTest "InsertChar inserts char"
        app
        insertChar
    <|
        \modelBeforeMsg msg finalModel ->
            case msg of
                InsertChar char ->
                    let
                        charString =
                            String.fromChar char

                        newLine =
                            lineContent finalModel.lines finalModel.cursor.line

                        oldLine =
                            lineContent modelBeforeMsg.lines modelBeforeMsg.cursor.line

                        beforeCursor =
                            String.left modelBeforeMsg.cursor.column oldLine

                        afterCursor =
                            String.dropLeft modelBeforeMsg.cursor.column oldLine
                    in
                    newLine
                        |> Expect.equal (beforeCursor ++ charString ++ afterCursor)

                _ ->
                    Expect.pass
