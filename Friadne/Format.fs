namespace Friadne

open System
open Friadne
open Friadne.Ansi
open Friadne.Symbols

/// Responsible for formatting diagnostic information into line-level representation (but not responsible for wrapping borders)
module Formatter =
  /// Level text (with color and style)
  let formatLevel =
    function
    | DiagnosticLevel.Error ->
      Ansi.colorAndStyle Ansi.Color.BrightRed Ansi.Style.Bold "Error"
    | DiagnosticLevel.Warning ->
      Ansi.colorAndStyle Ansi.Color.BrightYellow Ansi.Style.Bold "Warning"
    | DiagnosticLevel.Info ->
      Ansi.colorAndStyle Ansi.Color.BrightBlue Ansi.Style.Bold "Info"
    | DiagnosticLevel.Hint ->
      Ansi.colorAndStyle Ansi.Color.BrightCyan Ansi.Style.Bold "Hint"

  /// Format code (e.g., [FS01])
  let formatCode (code : DiagnosticCode) =
    Ansi.colorText Ansi.Color.BrightWhite $"[{code.Code}{code.Number:D2}]"

  /// Format location (file name and starting coordinates)
  let formatLocation (location : Location) =
    Ansi.colorText
      Ansi.Color.BrightBlack
      $"[{location.FileName}:({location.Range.Start.Line},{location.Range.Start.Column})]"

  /// Generate line number display (with pointer or empty space)
  let formatLineNumber (lineNum : int) (width : int) (isPointed : bool) =
    let paddedNum = (string lineNum).PadLeft width

    let symbol =
      if isPointed then Ansi.colorText Ansi.Color.BrightBlue "|->" else "   "

    Ansi.colorText Ansi.Color.BrightBlack paddedNum + " " + symbol + " "

  let private getAnnotationColor =
    function
    | DiagnosticLevel.Error -> Ansi.Color.BrightRed
    | DiagnosticLevel.Warning -> Ansi.Color.BrightYellow
    | DiagnosticLevel.Info -> Ansi.Color.BrightCyan
    | DiagnosticLevel.Hint -> Ansi.Color.BrightGreen

  let private createEmptyLine (width : int) = String (' ', width + 5)

  /// Annotation display level assignment: avoid overlap; the outer layer has the highest level
  let private assignDisplayLevels (annotations : Annotation list) =
    let sortedAnnotations =
      annotations |> List.sortBy (fun ann -> ann.Range.Start.Column)

    let rec assignLevels processed remaining =
      match remaining with
      | [] -> processed
      | ann :: rest ->
        let rec tryLevel level =
          let conflictsAtLevel =
            processed
            |> List.filter (fun (_, lvl) -> lvl = level)
            |> List.exists (fun (existing, _) ->
              let annStart = ann.Range.Start.Column
              let annEnd = ann.Range.End.Column
              let existingStart = existing.Range.Start.Column
              let existingEnd = existing.Range.End.Column

              let overlaps =
                not (annEnd < existingStart || annStart > existingEnd)

              let adjacent =
                annStart <= existingEnd + 1 && annEnd >= existingStart - 1

              overlaps || adjacent)

          if conflictsAtLevel then tryLevel (level + 1) else level

        let assignedLevel = tryLevel 0
        assignLevels ((ann, assignedLevel) :: processed) rest

    let result = assignLevels [] sortedAnnotations

    let maxLevel =
      if result.IsEmpty then 0 else result |> List.map snd |> List.max

    result |> List.map (fun (ann, level) -> ann, maxLevel - level)

  /// Render multi-level annotations in a single line (underline, vertical line, message connection)
  let private generateMultiLevelAnnotations
    (width : int)
    (annotations : Annotation list)
    =
    if annotations.IsEmpty then
      []
    else
      let severity =
        function
        | DiagnosticLevel.Error -> 4
        | DiagnosticLevel.Warning -> 3
        | DiagnosticLevel.Info -> 2
        | DiagnosticLevel.Hint -> 1

      let overallColor =
        annotations
        |> List.map (fun a -> a.Level)
        |> List.maxBy severity
        |> getAnnotationColor

      let annotationsWithLevels = assignDisplayLevels annotations
      let baseSpaces = createEmptyLine width

      // Underline layer
      let underlineLines () =
        let maxColumn =
          annotations |> List.map (fun ann -> ann.Range.End.Column) |> List.max

        let lineChars = Array.create maxColumn ' '

        annotationsWithLevels
        |> List.iter (fun (ann, level) ->
          let startCol = max 0 (ann.Range.Start.Column - 1)
          let endCol = max startCol (ann.Range.End.Column - 1)

          for j in startCol..endCol do
            if j < maxColumn then
              lineChars.[j] <-
                match level with
                | 0 -> Symbols.UnderlineInner
                | 1 -> Symbols.UnderlineMiddle
                | _ -> Symbols.UnderlineOuter)

        let underlineStr = String(lineChars).TrimEnd ()

        if underlineStr.Length > 0 then
          [ baseSpaces + Ansi.colorText overallColor underlineStr ]
        else
          []

      // Vertical line layer (pointer wrapping)
      let verticalLines () =
        let maxColumn =
          annotations |> List.map (fun ann -> ann.Range.End.Column) |> List.max

        let lineChars = Array.create maxColumn ' '

        annotationsWithLevels
        |> List.iter (fun (ann, level) ->
          let startCol = max 0 (ann.Range.Start.Column - 1)

          if startCol < maxColumn then
            lineChars.[startCol] <- Symbols.VerticalChar)

        let lineStr = String(lineChars).TrimEnd ()

        if lineStr.Length > 0 then
          [ baseSpaces + Ansi.colorText overallColor lineStr ]
        else
          []

      // Message layer (left bottom pointer + text)
      let messageLines () =
        let sorted =
          annotationsWithLevels
          |> List.sortBy snd
          |> List.choose (fun (ann, lvl) ->
            ann.Message |> Option.map (fun msg -> ann, lvl, msg))

        sorted
        |> List.mapi (fun index (ann, level, message) ->
          let maxColumn =
            annotations |> List.map (fun a -> a.Range.End.Column) |> List.max

          let lineChars = Array.create maxColumn ' '

          // Continue the vertical line of the outer annotation (before the current pointer)
          let remaining = sorted |> List.skip (index + 1)

          for (outerAnn, outerLevel, _) in remaining do
            let startCol = max 0 (outerAnn.Range.Start.Column - 1)
            let endCol = max 0 (outerAnn.Range.End.Column - 1)
            let currentPointerCol = max 0 (ann.Range.Start.Column - 1)

            if startCol < maxColumn && startCol <= currentPointerCol then
              lineChars.[startCol] <- Symbols.VerticalChar

            if
              outerLevel > 0
              && endCol < maxColumn
              && endCol <> startCol
              && endCol > currentPointerCol
            then
              lineChars.[endCol] <- Symbols.VerticalChar

          let pointerCol = max 0 (ann.Range.Start.Column - 1)

          let beforePointer =
            if pointerCol > 0 then
              String(lineChars.[0 .. (pointerCol - 1)])
                .Replace (
                  string Symbols.VerticalChar,
                  Ansi.colorText
                    (getAnnotationColor ann.Level)
                    (string Symbols.VerticalChar)
                )
            else
              ""

          let coloredPointer =
            Ansi.colorText
              (getAnnotationColor ann.Level)
              Symbols.BottomLeftPointer

          let coloredMessage = Ansi.colorText Ansi.Color.BrightWhite message
          baseSpaces + beforePointer + coloredPointer + " " + coloredMessage)

      let underlines = underlineLines ()
      let connectors = verticalLines ()
      let messages = messageLines ()
      underlines @ connectors @ messages

  /// Format single line of source code (including annotation line)
  let formatSourceLine
    (lines : string[])
    (lineNum : int)
    (width : int)
    (annotations : Annotation list)
    (level : DiagnosticLevel)
    =
    match SourceCode.getLine lines lineNum with
    | Some line ->
      let hasAnnotations = not annotations.IsEmpty
      let header = formatLineNumber lineNum width hasAnnotations
      let result = [ header + line ]

      if hasAnnotations then
        result @ generateMultiLevelAnnotations width annotations
      else
        result
    | None -> []
