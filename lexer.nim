import std/[
  strformat,
  strutils,
  streams,
  tables
]

type
  ZaphytLexingError* = object of CatchableError

  TokenType* = enum
    OpenParen, CloseParen, OpenBrace, CloseBrace, OpenBracket, CloseBracket
    Identifier, Keyword, String, Char, Int, Float, Comma, Dot, Colon, Semicolon, EOF
    # Here we have a way to tell the difference between a keyword and identifer to make lexing
    # and parsing less hellish in theory

  Token* = object
    typ*: TokenType
    startLine*, startColumn*: uint
    value*: string

  Lexer* = ref object
    stream*: Stream
    line*, column*: uint

const
  SimpleTokenMap = {
    ',': Comma,
    '.': Dot,
    ':': Colon,
    ';': Semicolon
  }.toTable

  BracketTokenMap = {
    '(': OpenParen,
    ')': CloseParen,
    '[': OpenBracket,
    ']': CloseBracket,
    '{': OpenBrace,
    '}': CloseBrace,
  }.toTable

  SpecialIdentifiers = ['+', '-', '*', '/', '%', '^']

  Unidentifiers = ".,:+-*/()[]{}\"'"

  Keywords = @[
    "func",
    "macro",
    "type",
    "object",
    "of",
    "ref",
    "static",
    "discard"
  ]


template atEnd(l: Lexer): bool = l.stream.atEnd()

template peek(l: Lexer): char = l.stream.peekChar()

proc next(l: Lexer) =
  ## Increments the column counter, and increments the line counter if
  ## a newline is encountered.
  if l.stream.readChar() == '\n':
    inc l.line
    l.column = 1
  else:
    inc l.column

proc newLexer*(stream: Stream): Lexer =
  ## Creates a new lexer from a stream.
  return Lexer(stream: stream, line: 1, column: 1)

proc lexStr(l: Lexer): Token =
  result = Token(typ: String, startLine: l.line, startColumn: l.column)
  var
    cchar = l.peek()
    pchar: char

  if cchar == '"':
    result.value &= cchar

  l.next()

  while not l.atEnd:
    pchar = cchar
    cchar = l.peek()

    if cchar == '\n':
      raise newException(ZaphytLexingError,
        fmt"Unterminated string literal at line {l.line} and column {l.column}!")

    result.value &= cchar

    if (cchar == '"') and (pchar != '\\'):
      l.next()
      break

    l.next()

  if (cchar != '"') or (cchar == '"' and pchar == '\\'):
    raise newException(ZaphytLexingError,
      fmt"Unterminated string literal at line {l.line} and column {l.column}, reached EOF!")


proc lexNum(l: Lexer): Token =
  result.typ = Int

  result.value &= l.peek()
  l.next()

  result.startLine = l.line
  result.startColumn = l.column

  var dotCount: uint8 = 0

  while not l.atEnd:
    let c = l.peek()

    if (dotCount == 1) and (c == '.'):
      break

    elif (c == '.') and (l.peek().isDigit):
      result.typ = Float
      inc dotCount

    elif (c in Whitespace) or (not c.isDigit):
      break

    result.value &= c

    l.next()


proc lexBackticks(l: Lexer): Token =
  var
    cchar = l.peek()
    pchar: char

  result = Token(typ: Identifier, startLine: l.line, startColumn: l.column)

  l.next()

  while not l.atEnd:
    pchar = cchar
    cchar = l.peek()

    if cchar == '\n':
      raise newException(ZaphytLexingError,
        fmt"Unterminated backtick at line {l.line} and column {l.column}!")

    elif cchar == '`':
      l.next()
      break

    else:
      result.value &= cchar

    l.next()

  if cchar != '`':
    raise newException(ZaphytLexingError,
      fmt"Unterminated backtick at line {l.line} and column {l.column}, reached EOF!")


proc lexIdent(l: Lexer): Token =
  var cchar = l.peek()

  result = Token(typ: Identifier, startLine: l.line, startColumn: l.column)
  result.value &= cchar

  l.next()

  while not l.atEnd:
    cchar = l.peek()

    if (cchar in Whitespace) or (cchar in Unidentifiers):
      if result.value in Keywords:
        result.typ = Keyword

      break

    result.value &= cchar

    l.next()

proc lexUntil(l: Lexer, cond: (proc(): bool) = nil): seq[Token] # Forward decl

proc lexBrackets(l: Lexer, b: tuple[open: char, close: char]): seq[Token] =
  # Used for lexing matching brackets before the parsing stage
  var cchar = l.peek()

  result.add Token(typ: BracketTokenMap[b.open], startLine: l.line,
    startColumn: l.column, value: $cchar)

  l.next()

  while not l.atEnd:
    cchar = l.peek()

    if cchar == b.close:
      result.add Token(typ: BracketTokenMap[b.close], startLine: l.line,
        startColumn: l.column, value: $cchar)
      l.next()
      break

    result.add l.lexUntil(proc(): bool = l.peek() != b.close)

  if cchar != b.close:
    raise newException(ZaphytLexingError,
      fmt"Unmatched `{b.open}` at line {l.line} and column {l.column}!")

proc lexChar(l: Lexer): Token =
  var
    cchar = l.peek()
    pchar: char

  result = Token(typ: Char, startLine: l.line, startColumn: l.column, value: $cchar)

  l.next()

  if l.atEnd:
    raise newException(ZaphytLexingError,
      fmt"Unterminated character literal at line {l.line} and column {l.column}!")

  else:
    pchar = cchar
    cchar = l.peek()

    if cchar == '\\':
      result.value &= cchar
      l.next()

      if l.atEnd:
        raise newException(ZaphytLexingError,
          fmt"Unterminated character literal at line {l.line} and column {l.column}!")

      else:
        pchar = cchar
        cchar = l.peek()

        if cchar == '\'' and pchar != '\\':
          raise newException(ZaphytLexingError,
            fmt"Unterminated character literal at line {l.line} and column {l.column}!")

        else:
          result.value &= cchar

          l.next()

    elif cchar == '\'':
      raise newException(ZaphytLexingError,
        fmt"Invalid character literal at line {l.line} and column {l.column}!")

    else:
      result.value &= cchar

      l.next()

  if l.atEnd:
    raise newException(ZaphytLexingError,
      fmt"Unterminated character literal at line {l.line} and column {l.column}!")

  else:
    pchar = cchar
    cchar = l.peek()

    if cchar != '\'':
      raise newException(ZaphytLexingError,
        fmt"Unterminated character literal at line {l.line} and column {l.column}!")

    else:
      result.value &= cchar

  l.next()

proc lexUntil(l: Lexer, cond: (proc(): bool) = nil): seq[Token] =
  template condition(): bool =
    if cond == nil:
      not l.atEnd
    else:
      cond() and not l.atEnd

  while condition():
    let cchar = l.peek()

    if cchar in Whitespace:
      l.next()

    elif cchar == '"':
      result.add l.lexStr()

    elif cchar == '`':
      result.add l.lexBackticks()

    elif cchar == '\'':
      result.add l.lexChar()

    elif cchar.isDigit():
      result.add l.lexNum()

    elif cchar == '(':
      result.add l.lexBrackets((open: '(', close: ')'))

    elif cchar == '[':
      result.add l.lexBrackets((open: '[', close: ']'))

    elif cchar == '{':
      result.add l.lexBrackets((open: '{', close: '}'))

    elif cchar in [')', ']', '}']:
      return

    elif cchar in SimpleTokenMap:
      result.add Token(startLine: l.line, startColumn: l.column, typ: SimpleTokenMap[cchar], value: $cchar)
      l.next()

    elif cchar in SpecialIdentifiers:
      var
        token = Token(startLine: l.line, startColumn: l.column, typ: Identifier)
        c: char
      token.value &= cchar

      l.next()

      while not l.atEnd:
        c = l.peek()
        l.next()
        if c in SpecialIdentifiers:
          token.value &= c

        else:
          break

      result.add token

    else:
      result.add l.lexIdent()

proc lex*(l: Lexer): seq[Token] =
  result = l.lexUntil()
  result.add Token(startLine: l.line, startColumn: l.column, typ: EOF, value: "<EOF>")