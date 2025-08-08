namespace Friadne

/// Line and column position in source code (1-based)
[<Struct>]
type Position =
  { Line : int
    Column : int }

/// Half-open half-closed interval [Start, End], both 1-based coordinates
[<Struct>]
type Range =
  { Start : Position
    End : Position }

/// File and main location range for diagnostics
[<Struct>]
type Location =
  { FileName : string
    Range : Range }

/// Diagnostic severity level (affects color and sorting)
type DiagnosticLevel =
  | Error
  | Warning
  | Info
  | Hint

/// Diagnostic code (Code + incrementing number)
[<Struct>]
type DiagnosticCode =
  { Code : string
    Number : int }

/// Diagnostic annotation, pointing to a range, can have custom message and level
[<Struct>]
type Annotation =
  { Range : Range
    Message : string option
    Level : DiagnosticLevel }

/// Diagnostic entity (title, main range, optional message/annotation/help)
[<Struct>]
type Diagnostic =
  { Code : DiagnosticCode
    Level : DiagnosticLevel
    Title : string
    Location : Location
    Message : string option
    Annotations : Annotation list
    Note : string option
    Help : string option }
