[<AutoOpen>]
module Diagnostics

type Level =
    | Error
    | Warning
    | Info
    | Hint

type Code = { Code: string; Number: int }



type Position = { Line: int; Column: int }
type Range = { Start: Position; End: Position }

type Annotation =
    { Range: Range
      Message: string option
      Level: Level }

type Location = { FileName: string; Range: Range }

type Diagnostic =
    { Code: Code
      Level: Level
      Title: string
      Location: Location
      Message: string option
      Annotations: Annotation list
      Note: string option
      Help: string option }
