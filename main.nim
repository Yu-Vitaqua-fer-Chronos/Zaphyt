import std/[
  streams
]

import ./[
  lexer
]

let code = newStringStream """
stdout.writeln "Hello, ", "World!"
stdout.write("1 + 1 = ", 1.uint8 + 1)
"""

var l = Lexer.new(code)

echo l.lex()