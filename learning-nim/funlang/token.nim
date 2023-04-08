type
  TokenType* = enum
    ttPlus, ttMinus, ttStar, ttSlash, ttEqual, ttGreater, ttLess,
    ttEqualEqual, ttGreaterEqual, ttLessEqual,
    ttLParen, ttRParen, ttLBrac, ttRBrac, ttLCurl, ttRCurl,
    ttNumberLiteral, ttBoolLiteral, ttStringLiteral, ttSymbol,
    ttKeyword,
    ttSpaceIndent, ttTabIndent, ttNewline, ttEOF, ttError, ttUnknown

type
  Token* = object
    typ*: TokenType
    val*: string
    col*: Natural
    line*: Natural
