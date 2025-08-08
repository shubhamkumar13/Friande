namespace Friadne

open System

/// ANSI color and style tools (can be replaced with other backends)
module Ansi =
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

  let private colorCode (color : Color) =
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
    | Style.Bold -> "1"
    | Style.Dim -> "2"
    | Style.Italic -> "3"
    | Style.Underline -> "4"
    | Style.Reset -> "0"

  /// Only color
  let colorText (color : Color) (text : string) =
    $"\u001b[{colorCode color}m{text}\u001b[0m"

  /// Only style
  let styleText (style : Style) (text : string) =
    $"\u001b[{styleCode style}m{text}\u001b[0m"

  /// Color + style
  let colorAndStyle (color : Color) (style : Style) (text : string) =
    $"\u001b[{styleCode style};{colorCode color}m{text}\u001b[0m"

  /// Get the display width after removing ANSI sequences
  let getDisplayWidth (text : string) =
    Text.RegularExpressions.Regex.Replace(text, @"\u001b\[[0-9;]*m", "").Length
