namespace Friadne

/// Symbols required for diagnostic rendering (centralized for easy replacement/localization)
module Symbols =
  [<Literal>]
  let VerticalChar = '│'

  [<Literal>]
  let HorizontalChar = '─'

  [<Literal>]
  let BottomLeftPointer = "└─"

  [<Literal>]
  let UnderlineInner = '^'

  [<Literal>]
  let UnderlineMiddle = '~'

  [<Literal>]
  let UnderlineOuter = '-'

  [<Literal>]
  let BoxTopLeft = '╭'

  [<Literal>]
  let BoxTopRight = '╮'

  [<Literal>]
  let BoxBottomLeft = '╰'

  [<Literal>]
  let BoxBottomRight = '╯'

  [<Literal>]
  let BoxHorizontal = '─'

  /// Left vertical border (with one space), used to wrap content
  [<Literal>]
  let BoxVerticalWithSpace = "│ "
