import std/[
  strutils,
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


proc lexNum(l: var Lexer): Token =
  result.value &= l.peek()
  l.next()

  result.startLine = l.line
  result.startColumn = l.column

  var dotCount: uint8 = 0

  while not l.atEnd:
    let c = l.peek()

    l.next()

    if (dotCount == 1) and (c == '.'):
      break

    elif c == '.':
      inc dotCount

    elif (c in Whitespace) and (not c.isDigit):
      break

    result.value &= c

  if '.' in result.value:
    result.typ = Float

  else:
    result.typ = Int


proc lex*(l: var Lexer): seq[Token] =
  while not l.atEnd():
    let cchar = l.peek()

    if cchar == '"':
      result.add l.lexStr()

    elif cchar.isDigit():
      result.add l.lexNum()

    else:
      l.next()