import std/os
import std/strutils
import std/unicode

import token

const usage = "Usage: flc FILE_NAME.fl"

proc isKeyword(symbol: string): bool =
  let keywords = @[
    "if",
    "else",
    "puts",
    "let",
  ]

  if symbol in keywords:
    return true
  return false

type 
  Lexer = object
    source: string
    offset: int
    line: int
    column: int

proc advance(l: var Lexer): bool = 
  if l.offset+1 >= len(l.source):
    return false
  inc(l.offset)
  inc(l.column)
  return true

proc peek(l: Lexer): (char, bool) =
  if l.offset+1 >= len(l.source):
    return (' ', false)
  return (l.source[l.offset+1], true)  

proc run(l: var Lexer): seq[Token] = 
  var tokens = newSeq[Token](0)
  var done = false
  while not done:
    let start = l.offset
    let c = l.source[l.offset]
    case c:
      of ' ':
        if l.column == 1:
          var count = 1
          while true:
            let (next, hasNext) = l.peek()
            if not hasNext:
              break
            if next != ' ':
              break
            discard l.advance()
            inc(count)
          tokens.add(Token(typ: ttSpaceIndent, val: $count, col: l.column, line: l.line))
        else:
          discard
      of '\t':
        # throw error message: NO TABS ALLOWED!!!
        discard
      of '\n':
        tokens.add(Token(typ: ttNewline, val: "Newline", col: l.column, line: l.line))
        l.column = 0
        inc(l.line)
      of '+':
        tokens.add(Token(typ: ttPlus, val: "+", col: l.column, line: l.line))
      of '-':
        tokens.add(Token(typ: ttMinus, val: "-", col: l.column, line: l.line))
      of '*':
        tokens.add(Token(typ: ttStar, val: "*", col: l.column, line: l.line))
      of '/':
        tokens.add(Token(typ: ttSlash, val: "/", col: l.column, line: l.line))
      of '=':
        tokens.add(Token(typ: ttEqual, val: "=", col: l.column, line: l.line))
      of '(':
        tokens.add(Token(typ: ttLParen, val: "(", col: l.column, line: l.line))
      of ')':
        tokens.add(Token(typ: ttRParen, val: ")", col: l.column, line: l.line))
      of '[':
        tokens.add(Token(typ: ttLBrac, val: "[", col: l.column, line: l.line))
      of ']':
        tokens.add(Token(typ: ttRBrac, val: "]", col: l.column, line: l.line))
      of '{':
        tokens.add(Token(typ: ttLCurl, val: "{", col: l.column, line: l.line))
      of '}':
        tokens.add(Token(typ: ttRCurl, val: "}", col: l.column, line: l.line))
      of '"':
        # StringLiteral (enclosed by quote chars)
        while l.advance():
          # TODO if string is not terminated within the same line it has been started
          #      => error token
          let (next, hasNext) = l.peek()
          if not hasNext:
            echo "[ERROR] unterminated string literal" 
            tokens.add(Token(typ: ttError, val: "unterminated string literal", col: l.column, line: l.line))
            break
          if next == '"':
            discard l.advance()
            break
        tokens.add(Token(typ: ttStringLiteral, val: l.source[start .. l.offset], col: l.column, line: l.line))
      else:
        # either one of: 
        #   - NumberLiteral (only containing digits)
        #   - BooleanLiteral (either 'true' or 'false')
        #   - Symbol (as defined by user or by compiler i.e. keyword)
        if c.isDigit():
          while true:
            let (next, hasNext) = l.peek()
            if not hasNext:
              break
            if not next.isDigit():
              break
            discard l.advance()
          tokens.add(Token(typ: ttNumberLiteral, val: l.source[start .. l.offset], col: l.column, line: l.line))
        elif isAlpha($c):
          while true:
            let (next, hasNext) = l.peek()
            if not hasNext:
              break
            if not isAlpha($next):
              break
            discard l.advance()
          let symbol = l.source[start .. l.offset]
          case symbol:
            of "true":
              tokens.add(Token(typ: ttBoolLiteral, val: symbol, col: l.column, line: l.line))
            of "false":
              tokens.add(Token(typ: ttBoolLiteral, val: symbol, col: l.column, line: l.line))
            else:
              if symbol.isKeyword():
                tokens.add(Token(typ: ttKeyword, val: symbol, col: l.column, line: l.line))
              else:
                tokens.add(Token(typ: ttSymbol, val: symbol, col: l.column, line: l.line))
        else:
          echo "found unknown"
          tokens.add(Token(typ: ttUnknown, val: l.source[start .. l.offset], col: l.column, line: l.line))
    done = not l.advance()
  tokens.add(Token(typ: ttEOF, val: "EOF", col: l.column, line: l.line))
  return tokens

when isMainModule:
  if paramCount() != 1:
    echo usage
    quit(64)

  let fileName = paramStr(1)
  let file = open(fileName)

  let source = file.readAll()

  var lexer = Lexer(
    source: source,
    offset: 0,
    line: 1,
    column: 1,
  )

  echo "-Lexer-----"
  echo lexer

  echo "\n-Tokens----"
  for token in lexer.run():
    echo token

  echo "\n-Lexer-----"
  echo lexer

  echo "source length: ", len(lexer.source)

# TODO fix column counting in lexer, tokens got referenced by their end not their start
# TODO add pattern matching capabilities
# TODO add two-char stuff (i.e. -=, += etc.)
# TODO add float support
# TODO better string literal parsing
