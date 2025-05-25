module Friadne

open System
open System.IO
open System.Text

module AnsiColors =
    type Color =
        | Black = 0
        | Red = 1
        | Green = 2
        | Yellow = 3
        | Blue = 4
        | Magenta = 5
        | Cyan = 6
        | White = 7
        | BrightBlack = 8
        | BrightRed = 9
        | BrightGreen = 10
        | BrightYellow = 11
        | BrightBlue = 12
        | BrightMagenta = 13
        | BrightCyan = 14
        | BrightWhite = 15

    type Style =
        | Bold
        | Dim
        | Italic
        | Underline
        | Reset

    let private colorCode (color: Color) =
        match color with
        | Color.Black -> "30"
        | Color.Red -> "31"
        | Color.Green -> "32"
        | Color.Yellow -> "33"
        | Color.Blue -> "34"
        | Color.Magenta -> "35"
        | Color.Cyan -> "36"
        | Color.White -> "37"
        | Color.BrightBlack -> "90"
        | Color.BrightRed -> "91"
        | Color.BrightGreen -> "92"
        | Color.BrightYellow -> "93"
        | Color.BrightBlue -> "94"
        | Color.BrightMagenta -> "95"
        | Color.BrightCyan -> "96"
        | Color.BrightWhite -> "97"
        | _ -> "37"

    let private styleCode =
        function
        | Bold -> "1"
        | Dim -> "2"
        | Italic -> "3"
        | Underline -> "4"
        | Reset -> "0"

    let colorText (color: Color) (text: string) =
        $"\u001b[{colorCode color}m{text}\u001b[0m"

    let styleText (style: Style) (text: string) =
        $"\u001b[{styleCode style}m{text}\u001b[0m"

    let colorAndStyle (color: Color) (style: Style) (text: string) =
        $"\u001b[{styleCode style};{colorCode color}m{text}\u001b[0m"

type Position = { Line: int; Column: int }
type Range = { Start: Position; End: Position }
type Location = { FileName: string; Range: Range }

type DiagnosticLevel =
    | Error
    | Warning
    | Info
    | Hint

type DiagnosticCode = { Code: string; Number: int }

type Annotation =
    { Range: Range
      Message: string option
      Level: DiagnosticLevel }

type Diagnostic =
    { Code: DiagnosticCode
      Level: DiagnosticLevel
      Title: string
      Location: Location
      Message: string option
      Annotations: Annotation list
      Note: string option
      Help: string option }

module SourceCode =

    let readSourceFile (fileName: string) : string[] = File.ReadAllLines fileName

    let getLine (lines: string[]) (lineNumber: int) : string option =
        if lineNumber >= 1 && lineNumber <= lines.Length then
            Some lines.[lineNumber - 1]
        else
            None

    /// Get the number of characters in the area used to display the line number.
    let getLineNumberWidth (endLineNumber: int) : int = max (string endLineNumber).Length 3

module Formatter =
    open AnsiColors

    let formatLevel =
        function
        | Error -> colorAndStyle Color.BrightRed Bold "Error"
        | Warning -> colorAndStyle Color.BrightYellow Bold "Warning"
        | Info -> colorAndStyle Color.BrightBlue Bold "Info"
        | Hint -> colorAndStyle Color.BrightCyan Bold "Hint"

    let formatCode (code: DiagnosticCode) =
        colorText Color.BrightWhite $"[{code.Code}{code.Number:D2}]"

    let formatLocation (location: Location) =
        colorText Color.BrightBlack $"[{location.FileName}:({location.Range.Start.Line},{location.Range.Start.Column})]"

    let generatePointer (column: int) (length: int) (symbol: string) =
        let spaces = String(' ', max 0 (column - 1))
        let pointers = String.replicate (max 1 length) symbol
        spaces + pointers

    let formatLineNumber (lineNum: int) (width: int) (isPointed: bool) =
        let numStr = string lineNum
        let paddedNum = numStr.PadLeft width

        let symbol =
            if isPointed then
                colorText Color.BrightBlue "|->"
            else
                "   "

        colorText Color.BrightBlack paddedNum + " " + symbol + " "

    let private getAnnotationColor =
        function
        | Error -> Color.BrightRed
        | Warning -> Color.BrightYellow
        | Info -> Color.BrightCyan
        | Hint -> Color.BrightGreen

    let private createEmptyLine (width: int) =
        let spaces = String(' ', width + 5)
        spaces

    /// Detect overlapping annotations and assign display levels (rightmost gets lowest level)
    let private assignDisplayLevels (annotations: Annotation list) =
        // Sort by start position (leftmost first for processing)
        let sortedAnnotations =
            annotations |> List.sortBy (fun ann -> ann.Range.Start.Column)

        // Assign levels: process from left to right, rightmost gets level 0
        let rec assignLevels processedWithLevels remaining =
            match remaining with
            | [] -> processedWithLevels
            | ann :: rest ->
                // Find what level this annotation should be at
                let findLevel () =
                    let rec tryLevel level =
                        let conflictsAtLevel =
                            processedWithLevels
                            |> List.filter (fun (_, lvl) -> lvl = level)
                            |> List.exists (fun (existing, _) ->
                                // Check if ranges overlap or are adjacent/nested
                                let annStart = ann.Range.Start.Column
                                let annEnd = ann.Range.End.Column
                                let existingStart = existing.Range.Start.Column
                                let existingEnd = existing.Range.End.Column

                                // Consider annotations as conflicting if:
                                // 1. They overlap: not (annEnd < existingStart || annStart > existingEnd)
                                // 2. One contains or is adjacent to the other for nesting effect
                                let overlaps = not (annEnd < existingStart || annStart > existingEnd)
                                let adjacent = annStart <= existingEnd + 1 && annEnd >= existingStart - 1
                                overlaps || adjacent)

                        if conflictsAtLevel then tryLevel (level + 1) else level

                    tryLevel 0

                let assignedLevel = findLevel ()
                assignLevels ((ann, assignedLevel) :: processedWithLevels) rest

        let result = assignLevels [] sortedAnnotations

        let maxLevel =
            if result.IsEmpty then
                0
            else
                result |> List.map snd |> List.max

        result |> List.map (fun (ann, level) -> ann, maxLevel - level)

    /// Generate multi-level annotation display with proper nesting effect
    let private generateMultiLevelAnnotations (width: int) (annotations: Annotation list) =
        if annotations.IsEmpty then
            []
        else
            let annotationsWithLevels = assignDisplayLevels annotations
            let baseSpaces = createEmptyLine width

            // Generate underline showing all ranges
            let generateUnderline () =
                let maxColumn =
                    annotations |> List.map (fun ann -> ann.Range.End.Column) |> List.max

                let lineChars = Array.create maxColumn ' '

                // Fill underlines for each annotation
                annotationsWithLevels
                |> List.iter (fun (ann, level) ->
                    let startCol = max 0 (ann.Range.Start.Column - 1)
                    let endCol = max startCol (ann.Range.End.Column - 1)

                    let underlineChar =
                        match level with
                        | 0 -> '^' // Innermost (rightmost)
                        | 1 -> '~' // Middle
                        | _ -> '-' // Outer

                    for j in startCol..endCol do
                        if j < maxColumn then
                            lineChars.[j] <- underlineChar)

                let underlineStr = String(lineChars).TrimEnd()

                if underlineStr.Length > 0 then
                    [ baseSpaces + colorText Color.BrightRed underlineStr ]
                else
                    []

            // Generate vertical connector lines
            let generateVerticalConnectors () =
                let maxColumn =
                    annotations |> List.map (fun ann -> ann.Range.End.Column) |> List.max

                let lineChars = Array.create maxColumn ' '

                // Draw vertical lines for all annotations
                annotationsWithLevels
                |> List.iter (fun (ann, level) ->
                    let startCol = max 0 (ann.Range.Start.Column - 1)

                    if level = 0 then
                        // Innermost: single vertical line at start
                        if startCol < maxColumn then
                            lineChars.[startCol] <- '│'
                    else if
                        // Outer levels: vertical lines at start and end to show "wrapping"
                        startCol < maxColumn
                    then
                        lineChars.[startCol] <- '│')

                let lineStr = String(lineChars).TrimEnd()

                if lineStr.Length > 0 then
                    [ baseSpaces + colorText Color.BrightCyan lineStr ]
                else
                    []

            // Generate message lines with proper connector continuation
            let generateMessages () =
                let sortedMessages =
                    annotationsWithLevels
                    |> List.sortBy snd // Sort by level (innermost first)
                    |> List.choose (fun (ann, level) ->
                        match ann.Message with
                        | Some message -> Some(ann, level, message)
                        | None -> None)

                sortedMessages
                |> List.mapi (fun index (ann, level, message) ->
                    let maxColumn = annotations |> List.map (fun a -> a.Range.End.Column) |> List.max
                    let lineChars = Array.create maxColumn ' '

                    // Only draw vertical lines for annotations that are still "active"
                    // An annotation is active if it hasn't been "closed" yet
                    let remainingAnnotations = sortedMessages |> List.skip (index + 1)

                    remainingAnnotations
                    |> List.iter (fun (outerAnn, outerLevel, _) ->
                        let startCol = max 0 (outerAnn.Range.Start.Column - 1)
                        let endCol = max 0 (outerAnn.Range.End.Column - 1)

                        // Only draw continuation lines if past the start of this annotation
                        let currentPointerCol = max 0 (ann.Range.Start.Column - 1)

                        // Continue vertical lines from outer annotations, but only if within their range
                        if startCol < maxColumn && startCol <= currentPointerCol then
                            lineChars.[startCol] <- '│'

                        if
                            outerLevel > 0
                            && endCol < maxColumn
                            && endCol <> startCol
                            && endCol > currentPointerCol
                        then
                            lineChars.[endCol] <- '│')

                    let pointer = "└─"
                    let pointerCol = max 0 (ann.Range.Start.Column - 1)

                    let beforePointer =
                        if pointerCol > 0 then
                            let beforePointerStr = String(lineChars.[0 .. (pointerCol - 1)])
                            beforePointerStr.Replace("│", colorText Color.BrightCyan "│")
                        else
                            ""

                    let coloredPointer = colorText (getAnnotationColor ann.Level) pointer
                    let coloredMessage = colorText Color.BrightWhite message

                    baseSpaces + beforePointer + coloredPointer + " " + coloredMessage)

            // Combine all parts in correct order
            let underlines = generateUnderline ()
            let connectors = generateVerticalConnectors ()
            let messages = generateMessages ()

            underlines @ connectors @ messages

    /// Format Source Code Line with improved multi-level annotations
    let formatSourceLine (lines: string[]) (lineNum: int) (width: int) (annotations: Annotation list) =
        match SourceCode.getLine lines lineNum with
        | Some line ->
            let hasAnnotations = not annotations.IsEmpty
            let lineHeader = formatLineNumber lineNum width hasAnnotations
            let formattedLine = lineHeader + line

            let result = [ formattedLine ]

            if hasAnnotations then
                // Generate multi-level annotation display
                let annotationLines = generateMultiLevelAnnotations width annotations
                result @ annotationLines
            else
                result
        | None -> []

/// Main Diagnostic Renderer
module Renderer =
    open AnsiColors
    open Formatter

    /// Remove ANSI escape sequences to get actual display width
    let private getDisplayWidth (text: string) =
        System.Text.RegularExpressions.Regex.Replace(text, @"\u001b\[[0-9;]*m", "").Length

    /// Pad line to fixed width and wrap with box borders
    let private wrapWithBorder (content: string) (maxWidth: int) =
        let displayWidth = getDisplayWidth content + 1
        let padding = max 0 (maxWidth - displayWidth)
        let paddedContent = content + String(' ', padding)
        "│ " + paddedContent

    /// Render Full Diagnostic Message
    let renderDiagnostic (diagnostic: Diagnostic) : string =
        let sb = StringBuilder()
        let lines = SourceCode.readSourceFile diagnostic.Location.FileName
        let boxWidth = 80 // Fixed width for the diagnostic box

        // Start building content lines
        let contentLines = ResizeArray<string>()

        // Add Title Line
        let header =
            sprintf "%s %s: %s" (formatCode diagnostic.Code) (formatLevel diagnostic.Level) diagnostic.Title

        contentLines.Add header

        // Add Location Information
        let location = "└─ " + formatLocation diagnostic.Location
        contentLines.Add location
        contentLines.Add "" // Empty line

        // Add Source Code and Annotations
        let range = diagnostic.Location.Range
        let startLine = max 1 (range.Start.Line - 1)
        let endLine = min lines.Length (range.End.Line + 1)
        let lineWidth = SourceCode.getLineNumberWidth endLine

        // Group annotations by line number
        let annotationsByLine =
            diagnostic.Annotations
            |> List.groupBy (fun ann -> ann.Range.Start.Line)
            |> Map.ofList

        // Add Code Lines
        for lineNum in startLine..endLine do
            let lineAnnotations =
                annotationsByLine |> Map.tryFind lineNum |> Option.defaultValue []

            let sourceLines = formatSourceLine lines lineNum lineWidth lineAnnotations
            sourceLines |> List.iter contentLines.Add

        contentLines.Add ""

        // Add Note
        match diagnostic.Note with
        | Some note ->
            let noteHeader = colorAndStyle Color.BrightBlue Bold "Note"
            contentLines.Add $"○ {noteHeader}: {note}"
        | None -> ()

        // Add Help Information
        match diagnostic.Help with
        | Some help ->
            let helpHeader = colorAndStyle Color.BrightGreen Bold "Help"
            contentLines.Add $"○ {helpHeader}: {help}"
        | None -> ()

        // Remove trailing empty lines
        while contentLines.Count > 0
              && String.IsNullOrWhiteSpace(contentLines.[contentLines.Count - 1]) do
            contentLines.RemoveAt(contentLines.Count - 1)

        // Build the final output with box borders
        let topBorder = "╭" + String('─', boxWidth) + "╮"
        let bottomBorder = "╰" + String('─', boxWidth) + "╯"

        sb.AppendLine topBorder |> ignore

        // Add each content line wrapped with borders
        for contentLine in contentLines do
            let wrappedLine = wrapWithBorder contentLine boxWidth
            sb.AppendLine wrappedLine |> ignore

        sb.AppendLine bottomBorder |> ignore
        sb.AppendLine() |> ignore // Add space between diagnostics

        sb.ToString()

/// Diagnostics Module
module Diagnostics =

    /// Create Error Diagnostic
    let createError (code: string) (number: int) (title: string) (location: Location) =
        { Code = { Code = code; Number = number }
          Level = Error
          Title = title
          Location = location
          Message = None
          Annotations = []
          Note = None
          Help = None }

    /// Create Warning Diagnostic
    let createWarning (code: string) (number: int) (title: string) (location: Location) =
        { Code = { Code = code; Number = number }
          Level = Warning
          Title = title
          Location = location
          Message = None
          Annotations = []
          Note = None
          Help = None }

    /// Add Annotation
    let withAnnotation (range: Range) (message: string) (level: DiagnosticLevel) (diagnostic: Diagnostic) =
        let annotation =
            { Range = range
              Message = Some message
              Level = level }

        { diagnostic with
            Annotations = annotation :: diagnostic.Annotations }

    /// Add Note
    let withNote (note: string) (diagnostic: Diagnostic) = { diagnostic with Note = Some note }

    /// Add Help Information
    let withHelp (help: string) (diagnostic: Diagnostic) = { diagnostic with Help = Some help }

    /// Render Diagnostic to String
    let render (diagnostic: Diagnostic) : string = Renderer.renderDiagnostic diagnostic

    /// Print Diagnostic to Console
    let print (diagnostic: Diagnostic) =
        let output = render diagnostic
        Console.WriteLine output
