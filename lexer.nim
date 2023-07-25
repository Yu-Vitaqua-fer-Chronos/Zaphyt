import std/[
  streams
]

type
  TokenType* = enum
    OpenParam, CloseParam, OpenBrace, CloseBrace, OpenBracket, CloseBracket
    String, Char, Int, Float, Comma, Dot, Semicolon, EOF

  Token* = object
    typ*: TokenType
    startLine*, startColumn*: uint
    value*: string

  Lexer* = object
    stream*: Stream
    line*, column*: uint


template atEnd(l: Lexer): bool = l.stream.atEnd()

template peek(l: Lexer): char = l.stream.peekChar()

proc next(l: var Lexer) =
  ## Increments the line and column
  if l.stream.readChar() == '\n':
    inc l.line
    l.column = 0
  else:
    inc l.column


proc new*(_: typedesc[Lexer], stream: Stream): Lexer =
  ## `new` proc for creating the lexer object
  result.stream = stream
  result.line = 0
  result.column = 0


proc lexStr(l: var Lexer): Token =
  var pchar = l.peek()

  if pchar == '"':
    result.value &= pchar

  l.next()

  result.typ = String
  result.startLine = l.line
  result.startColumn = l.column

  while not l.atEnd:
    let c = l.peek()

    l.next()

    result.value &= c
    pchar = c

    if (c == '"') and (pchar != '\\'):
      break


proc lex*(l: var Lexer): seq[Token] =
  while not l.atEnd():
    let cchar = l.peek()

    if cchar == '"':
      result.add l.lexStr()

    else:
      l.next()