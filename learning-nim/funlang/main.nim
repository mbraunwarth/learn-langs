import std/os
import std/strutils
import std/unicode

import token

const usage = "Usage: flc FILE_NAME.fl"

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
      of ' ', '\t':
        discard
      of '\n':
        l.column = 0
        inc(l.line)
      of '+':
        tokens.add(Token(typ: ttPlus, val: "+"))
      of '-':
        tokens.add(Token(typ: ttMinus, val: "-"))
      of '*':
        tokens.add(Token(typ: ttStar, val: "*"))
      of '/':
        tokens.add(Token(typ: ttSlash, val: "/"))
      of '=':
        tokens.add(Token(typ: ttEqual, val: "="))
      of '(':
        tokens.add(Token(typ: ttLParen, val: "("))
      of ')':
        tokens.add(Token(typ: ttRParen, val: ")"))
      of '[':
        tokens.add(Token(typ: ttLBrac, val: "["))
      of ']':
        tokens.add(Token(typ: ttRBrac, val: "]"))
      of '{':
        tokens.add(Token(typ: ttLCurl, val: "{"))
      of '}':
        tokens.add(Token(typ: ttRCurl, val: "}"))
      of '"':
        # StringLiteral (enclosed by quote chars)
        while l.advance():
          let (next, hasNext) = l.peek()

          if not hasNext:
            echo "[ERROR] unterminated string literal"
            tokens.add(Token(typ: ttError, val: "unterminated string literal"))
            break

          if next == '"':
            discard l.advance()
            break

        tokens.add(Token(typ: ttStringLiteral, val: l.source[start .. l.offset]))
      else:
        # either one of: 
        #   - NumberLiteral (only containing digits)
        #   - BooleanLiteral (either 'true' or 'false')
        #   - Symbol (as defined by user or by compiler i.e. keyword)
        if c.isDigit():
          echo "found digit"
          while true:
            let (next, hasNext) = l.peek()
            if not hasNext:
              break
            if not next.isDigit():
              break
            discard l.advance()
          tokens.add(Token(typ: ttNumberLiteral, val: l.source[start .. l.offset]))
        elif isAlpha($c):
          while true:
            let (next, hasNext) = l.peek()
            if not hasNext:
              break
            if not isAlpha($next):
              break
            discard l.advance()
          tokens.add(Token(typ: ttSymbol, val: l.source[start .. l.offset]))
        else:
          echo "found unknown"
          tokens.add(Token(typ: ttUnknown, val: l.source[start .. l.offset]))
    done = not l.advance()
  tokens.add(Token(typ: ttEOF, val: "EOF"))
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
