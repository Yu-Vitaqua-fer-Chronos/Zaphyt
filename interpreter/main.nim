import std/[
  streams,
  os
]

import ./[
  parser,
  lexer
]

let fileName = paramStr(1)

var code: Stream

if fileName == "-":
  code = newFileStream(stdin)

elif fileName == "":
  raise OSError.newException("No file was provided to be interpreted!")

else:
  code = newFileStream(fileName, fmRead)

if code == nil:
  raise OSError.newException("The file could not be opened!")

var l = Lexer.new(code)

let tokens = l.lex()

discard tokens.parse()