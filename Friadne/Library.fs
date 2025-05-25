module Friadne

open System
open System.IO
open System.Text

module AnsiColors = AnsiColors
module Diagnostics = Diagnostics
module SourceCode = SourceCode
module Formatter = Formatter

/// Main Diagnostic Renderer
module Renderer =
    open AnsiColors
    open Formatter

    /// Remove ANSI escape sequences to get actual display width
    let private getDisplayWidth (text: string) =
        System.Text.RegularExpressions.Regex.Replace(text, """\u001b\[[0-9;]*m""", "").Length

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
