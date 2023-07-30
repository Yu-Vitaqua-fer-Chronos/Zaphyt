import std/[
  streams
]

import ./[
  lexer
]

let code = newFileStream "main.zhy"

var l = Lexer.new(code)

echo l.lex()
