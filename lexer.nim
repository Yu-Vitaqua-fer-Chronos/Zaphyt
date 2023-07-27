import std/[
  strutils,
  streams
]

const Unidentifiers = ".,()[]{}"

type
  TokenType* = enum
    OpenParen, CloseParen, OpenBrace, CloseBrace, OpenBracket, CloseBracket
    Identifier, String, Char, Int, Float, Comma, Dot, Semicolon, EOF

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

    elif (c == '.') and (l.peek().isDigit):
      inc dotCount

    elif (c in Whitespace) or (not c.isDigit):
      break

    result.value &= c

  if '.' in result.value:
    result.typ = Float

  else:
    result.typ = Int


proc lexIdent(l: var Lexer): Token =
  var cchar = l.peek()

  l.next()

  if cchar != '`':
    result.value &= cchar

  result.startLine = l.line
  result.startColumn = l.column
  result.typ = Identifier

  if cchar == '`':
    while not l.atEnd:
      cchar = l.peek()

      l.next()

      if cchar == '`':
        # Don't want to include the backtick in the identifier
        break

      else:
        result.value &= cchar

  else:
    while not l.atEnd:
      cchar = l.peek()

      l.next()

      if (cchar in Whitespace) or (cchar in Unidentifiers):
        break

      result.value &= cchar


proc lex*(l: var Lexer): seq[Token] =
  while not l.atEnd():
    let cchar = l.peek()

    if cchar in Whitespace:
      l.next()

    elif cchar == '"':
      result.add l.lexStr()

    elif cchar.isDigit():
      result.add l.lexNum()

    elif cchar == ',':
      result.add Token(startLine: l.line, startColumn: l.column, typ: Comma, value: ",")
      l.next()

    elif cchar == '.':
      result.add Token(startLine: l.line, startColumn: l.column, typ: Dot, value: ".")
      l.next()

    elif cchar == '(':
      result.add Token(startLine: l.line, startColumn: l.column, typ: OpenParen, value: "(")
      l.next()

    elif cchar == ')':
      result.add Token(startLine: l.line, startColumn: l.column, typ: CloseParen, value: ")")
      l.next()

    elif cchar == '[':
      result.add Token(startLine: l.line, startColumn: l.column, typ: OpenBracket, value: "[")
      l.next()

    elif cchar == ']':
      result.add Token(startLine: l.line, startColumn: l.column, typ: CloseBracket, value: "]")
      l.next()

    elif cchar == '{':
      result.add Token(startLine: l.line, startColumn: l.column, typ: OpenBrace, value: "{")
      l.next()

    elif cchar == '}':
      result.add Token(startLine: l.line, startColumn: l.column, typ: CloseBrace, value: "}")
      l.next()

    else:
      result.add l.lexIdent()

  result.add Token(startLine: l.line, startColumn: l.column, typ: EOF, value: "<EOF>")
