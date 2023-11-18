import std/[
  strformat,
  strutils
]

import ./[
  lexer
]

type
  ZaphytParsingError* = object of CatchableError

  Parser* = ref object
    tokens: seq[Token]
    pos: int

  Node* = ref object of RootObj

  Block* = ref object of Node
    children*: seq[Node]

  Expression* = ref object of Node

  Statement* = ref object of Node

  LiteralKind* = enum
    Int8, Int16, Int32, Int64, Uint8, Uint16, Uint32, Uint64,
    String, Char, Float32, Float64, Byte

  LiteralExpr* = ref object of Expression
    case kind*: LiteralKind
      of {Int8, Int16, Int32, Int64}:
        intVal*: int64

      of {Uint8, Uint16, Uint32, Uint64}:
        uintVal*: uint64

      of {Float32, Float64}:
        floatVal*: float64

      of Byte:
        byteVal*: byte

      of Char:
        charVal*: char

      of String:
        strVal*: string

  IdentifierExpr* = ref object of Expression
    ident*: string

  FunctionCallExpr* = ref object of Expression
    ident*: Expression
    args*: seq[Expression]

  DotExpr* = ref object of Expression
    left*: Expression
    right*: Expression

proc newParser*(tokens: seq[Token]): Parser =
  return Parser(tokens: tokens, pos: 0)

proc parseExpr(p: Parser): Expression =
  return case p.tokens[p.pos].typ
    of Identifier:
      IdentifierExpr(ident: p.tokens[p.pos].value)

    of Int:
      try:
        LiteralExpr(kind: Int64, intVal: parseInt(p.tokens[p.pos].value))
      except ValueError:
        try:
          LiteralExpr(kind: Uint64, uintVal: parseUint(p.tokens[p.pos].value))

        except ValueError:
          try:
            LiteralExpr(kind: Float64, floatVal: parseFloat(p.tokens[p.pos].value))

          except ValueError:
            raise newException(ZaphytParsingError, "Could not parse integer at " &
              &"line {p.tokens[p.pos].startLine} and column {p.tokens[p.pos].startColumn}!")

    of Float:
      try:
        LiteralExpr(kind: Float64, floatVal: parseFloat(p.tokens[p.pos].value))

      except ValueError:
        raise newException(ZaphytParsingError, "Could not parse float at " &
          & "line {p.tokens[p.pos].startLine} and column {p.tokens[p.pos].startColumn}!")

    of String:
      LiteralExpr(kind: String, strVal: p.tokens[p.pos].value.substr(1, p.tokens[p.pos].value.len - 2))

    of Char:
      LiteralExpr(kind: Char, charVal: p.tokens[p.pos].value.substr(1, p.tokens[p.pos].value.len - 2)[0])

    else:
      raise newException(ZaphytParsingError, "Could not parse expression at " &
        & "line {p.tokens[p.pos].startLine} and column {p.tokens[p.pos].startColumn}!")


proc literalParseDotExpr(p: Parser): DotExpr =
  result = DotExpr()

  result.left = p.parseExpr()
  p.pos += 2
  result.right = p.parseExpr()


proc literalParsingStage(p: Parser): Block =
  result = Block()

  while p.pos < p.tokens.len:
    if p.tokens[p.pos].typ in {Identifier, String, Int, Float, Char}:
      if p.tokens[p.pos + 1].typ == Dot:
        result.children.add p.literalParseDotExpr()

    inc p.pos

# TODO: Return a `Module` rather than a `Block`
proc parse*(p: Parser): Block =
  result = p.literalParsingStage()