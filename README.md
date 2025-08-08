<div align="center">

![screenshot](./.github/Screenshot%20from%202025-05-25%2016-41-02.png)

# Friadne

*Simple and zero-deps Compiler Diagnostic Library for F#*

[![NuGet version](https://badge.fury.io/nu/Friadne.svg)](https://badge.fury.io/nu/Friadne) [![Build and Test](https://github.com/muqiuhan/Friande/actions/workflows/ci.yml/badge.svg)](https://github.com/muqiuhan/Friande/actions/workflows/ci.yml)

</div>

## How to Use

1.  **Define a Location**: Specify the file and the primary range for the diagnostic.
2.  **Create a Diagnostic**: Use `Diagnostics.createError`, `Diagnostics.createWarning`, etc.
3.  **Add Annotations**: Use `Diagnostics.withAnnotation` to highlight specific code segments within the diagnostic's range and provide messages for them. Annotations can have their own severity level, influencing their color.
4.  **Add Notes and Help**: Use `Diagnostics.withNote` and `Diagnostics.withHelp` for additional information.
5.  **Render or Print**: Use `Diagnostics.render` to get the string representation or `Diagnostics.print` to output directly to the console.

### Example:

Here's how you might define an error diagnostic with multiple annotations on a single line and a note:

```fsharp
open Friadne

// Assuming 'demo.fsx' is the file being analyzed
let location =
    { FileName = "demo.fsx"
      Range =
        { Start = { Line = 7; Column = 20 } // Primary range for the error
          End = { Line = 7; Column = 30 } } }

let diagnostic =
    Diagnostics.createError "FS" 0001 "Type error" location
    |> Diagnostics.withAnnotation
        { Start = { Line = 7; Column = 20 } // First annotation
          End = { Line = 7; Column = 29 } }
        "This expression was expected to have type 'a array"
        DiagnosticLevel.Info // Annotations can have their own level for styling
    |> Diagnostics.withAnnotation
        { Start = { Line = 7; Column = 30 } // Second annotation
          End = { Line = 7; Column = 30 } }
        "but here has type 'a list"
        DiagnosticLevel.Info
    |> Diagnostics.withNote "Incompatible types"

// To print to console:
// Diagnostics.print diagnostic

// To get as a string:
// let outputString = Diagnostics.render diagnostic
```

## Running the Demo Project

To see Friadne in action with more examples, you can run the test project:

```bash
dotnet run --project Friadne.Test -- demo.fsx
```

## [LICENSE](./LICENSE)

```
Copyright (c) 2025 Somhairle H. Marisol

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the name of Fringer nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```