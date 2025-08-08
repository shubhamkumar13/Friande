namespace Friadne

/// Public diagnostic building and rendering API (stable external interface)
module Diagnostics =
  /// Create Error-level diagnostic
  let createError
    (code : string)
    (number : int)
    (title : string)
    (location : Location)
    =
    { Code =
        { Code = code
          Number = number }
      Level = DiagnosticLevel.Error
      Title = title
      Location = location
      Message = None
      Annotations = []
      Note = None
      Help = None }

  /// Create Warning-level diagnostic
  let createWarning
    (code : string)
    (number : int)
    (title : string)
    (location : Location)
    =
    { Code =
        { Code = code
          Number = number }
      Level = DiagnosticLevel.Warning
      Title = title
      Location = location
      Message = None
      Annotations = []
      Note = None
      Help = None }

  /// Add annotation
  let withAnnotation
    (range : Range)
    (message : string)
    (level : DiagnosticLevel)
    (diagnostic : Diagnostic)
    =
    let annotation =
      { Range = range
        Message = Some message
        Level = level }

    { diagnostic with Annotations = annotation :: diagnostic.Annotations }

  /// Add Note
  let withNote (note : string) (diagnostic : Diagnostic) =
    { diagnostic with Note = Some note }

  /// Add Help
  let withHelp (help : string) (diagnostic : Diagnostic) =
    { diagnostic with Help = Some help }

  /// Render to string
  let render (diagnostic : Diagnostic) : string =
    Renderer.renderDiagnostic diagnostic

  /// Print to console
  let print (diagnostic : Diagnostic) =
    let output = render diagnostic
    System.Console.WriteLine output
