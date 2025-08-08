namespace Friadne

open System
open System.IO

module SourceCode =
  /// Read complete source file into array (one element per line)
  let readSourceFile (fileName : string) : string[] = File.ReadAllLines fileName

  /// Get specified line (1-based). Return None if out of bounds.
  let getLine (lines : string[]) (lineNumber : int) : string option =
    if lineNumber >= 1 && lineNumber <= lines.Length then
      Some lines.[lineNumber - 1]
    else
      None

  /// Calculate the width used for displaying line numbers (at least 3)
  let getLineNumberWidth (endLineNumber : int) : int =
    max (string endLineNumber).Length 3
