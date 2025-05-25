module Program

open System
open Friadne

[<EntryPoint>]
let main argv =
    let location =
        { FileName = argv.[0]
          Range =
            { Start = { Line = 7; Column = 20 }
              End = { Line = 7; Column = 30 } } }

    Diagnostics.createError "FS" 0001 "Type error" location
    |> Diagnostics.withAnnotation
        { Start = { Line = 7; Column = 20 }
          End = { Line = 7; Column = 29 } }
        "This expression was expected to have type 'a array"
        DiagnosticLevel.Info
    |> Diagnostics.withAnnotation
        { Start = { Line = 7; Column = 30 }
          End = { Line = 7; Column = 30 } }
        "but here has type 'a list"
        DiagnosticLevel.Info
    |> Diagnostics.withNote "Incompatible types"
    |> Diagnostics.print

    let location =
        { FileName = argv.[0]
          Range =
            { Start = { Line = 18; Column = 5 }
              End = { Line = 18; Column = 5 } } }

    Diagnostics.createWarning "FS" 0002 "Unused variable" location
    |> Diagnostics.withAnnotation
        { Start = { Line = 18; Column = 5 }
          End = { Line = 18; Column = 5 } }
        "Variable 'x' is never used"
        DiagnosticLevel.Warning
    |> Diagnostics.withHelp "Consider removing this variable or using it in your code"
    |> Diagnostics.print

    let location =
        { FileName = argv.[0]
          Range =
            { Start = { Line = 2; Column = 9 }
              End = { Line = 20; Column = 62 } } }

    Diagnostics.createError "FS" 0001 "Type error" location
    |> Diagnostics.withAnnotation
        { Start = { Line = 2; Column = 9 }
          End = { Line = 2; Column = 19 } }
        "This expression was expected to have type 'a list list"
        DiagnosticLevel.Info
    |> Diagnostics.withAnnotation
        { Start = { Line = 20; Column = 23 }
          End = { Line = 20; Column = 62 } }
        "but here has type 'b array"
        DiagnosticLevel.Info
    |> Diagnostics.withNote "Incompatible types"
    |> Diagnostics.print

    0
