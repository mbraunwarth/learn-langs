import std/os

import token

const usage = "Usage: flc FILE_NAME.fl"

type 
  Lexer = object
    source: string
    offset: int
    line: int
    column: int

proc advance(l: var Lexer): bool = 
  inc(l.offset)
  if l.offset >= len(l.source):
    return false
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
    let c = l.source[l.offset]:
    case c:
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
      of '"':
        # StringLiteral (enclosed by quote chars)
      else:
        # either one of: 
        #   - NumberLiteral (only containing digits)
        #   - BooleanLiteral (either 'true' or 'false')
        #   - Symbol (as defined by user or by compiler i.e. keyword)
        if c.isDigit():
        elseif c.isAlpha():
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
