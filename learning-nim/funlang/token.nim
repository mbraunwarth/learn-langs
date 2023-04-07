type
  TokenType* = enum
    ttPlus, ttMinus, ttStar, ttSlash, ttEqual, ttGreater, ttLess,
    ttEqualEqual, ttGreaterEqual, ttLessEqual,
    ttEOF, ttUnknown

type
  Token* = object
    typ*: TokenType
    val*: string

