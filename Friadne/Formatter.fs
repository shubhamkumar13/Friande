module Formatter

open AnsiColors
open Diagnostics
open System

let formatLevel =
    function
    | Error -> colorAndStyle Color.BrightRed Bold "Error"
    | Warning -> colorAndStyle Color.BrightYellow Bold "Warning"
    | Info -> colorAndStyle Color.BrightBlue Bold "Info"
    | Hint -> colorAndStyle Color.BrightCyan Bold "Hint"

let formatCode (code: Code) =
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
    let sortedAnnotations: Annotation list =
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
                        String(lineChars.[0 .. (pointerCol - 1)])
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
