import std/os

import lexer

const usage = "Usage: flc FILE_NAME.fl"

when isMainModule:
  if paramCount() != 1:
    echo usage
    quit(64)

  let fileName = paramStr(1)
  let file = open(fileName)

  let source = file.readAll()

  var l = Lexer(
    source: source,
    offset: 0,
    line: 1,
    column: 1,
  )

  echo "-Lexer-----"
  echo l

  echo "\n-Tokens----"
  for token in l.run():
    echo token

  echo "\n-Lexer-----"
  echo l

  echo "source length: ", len(l.source)

# TODO fix column counting in lexer, tokens got referenced by their end not their start
# TODO add pattern matching capabilities
# TODO add two-char stuff (i.e. -=, += etc.)
# TODO add float support
# TODO better string literal parsing
