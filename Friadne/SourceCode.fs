module SourceCode

open System.IO

let readSourceFile (fileName: string) : string array = File.ReadAllLines fileName

let getLine (lines: string array) (lineNumber: int) : string option =
    if lineNumber >= 1 && lineNumber <= lines.Length then
        Some lines.[lineNumber - 1]
    else
        None

/// Get the number of characters in the area used to display the line number.
let getLineNumberWidth (endLineNumber: int) : int = max (string endLineNumber).Length 3
