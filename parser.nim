import std/[
  strformat,
  strutils
]

import ./[
  lexer
]

const IndentStr = "  "

type
  ZaphytDefect* = object of Defect

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
    String, Char, Float32, Float64

  LiteralExpr* = ref object of Expression
    case kind*: LiteralKind
      of {Int8, Int16, Int32, Int64}:
        intVal*: int64

      of {Uint8, Uint16, Uint32, Uint64}:
        uintVal*: uint64

      of {Float32, Float64}:
        floatVal*: float64

      of Char:
        charVal*: char

      of String:
        strVal*: string

  IdentifierExpr* = ref object of Expression
    ident*: string

  FunctionCallExpr* = ref object of Expression
    callee*: Expression
    args*: seq[Expression]

  PathExpr* = ref object of Expression
    # Really, only the first expression can be an expression, everything else has to be an identifier
    expressions*: seq[Expression]


template curToken(p: Parser): Token = p.tokens[p.pos]

func toTree(n: Node, level: var int): string

func toTree(n: Block, level: var int): string =
  result &= repeat(IndentStr, level) & "Block(\n"
  inc level

  for idx, child in n.children:
    result &= repeat(IndentStr, level) & child.toTree(level)

    if idx < n.children.len - 1:
      result &= ",\n"

  dec level
  result &= '\n' & repeat(IndentStr, level) & ")"

func toTree(n: LiteralExpr, level: var int): string =
  case n.kind
    of Int8: result &= &"IntLiteral {n.intVal}'i8"
    of Int16: result &= &"IntLiteral {n.intVal}'i16"
    of Int32: result &= &"IntLiteral {n.intVal}'i32"
    of Int64: result &= &"IntLiteral {n.intVal}'i64"
    of Uint8: result &= &"UintLiteral {n.uintVal}'u8"
    of Uint16: result &= &"UintLiteral {n.uintVal}'u16"
    of Uint32: result &= &"UintLiteral {n.uintVal}'u32"
    of Uint64: result &= &"UintLiteral {n.uintVal}'u64"
    of Float32: result &= &"FloatLiteral {n.floatVal}'f32"
    of Float64: result &= &"FloatLiteral {n.floatVal}'f64"

    of Char:
      result &= "CharLiteral "
      result.addQuoted(n.charVal)

    of String:
      result &= "StrLiteral "
      result.addQuoted(n.strVal)

func toTree(n: IdentifierExpr, level: var int): string =
  result &= &"Identifier `{n.ident}`"

func toTree(n: FunctionCallExpr, level: var int): string =
  result &= "FunctionCall(\n"

  inc level
  result &= repeat(IndentStr, level) & "caller: " & n.callee.toTree(level) & ",\n"
  result &= repeat(IndentStr, level) & "args: [\n"

  inc level

  for idx, arg in n.args:
    result &= repeat(IndentStr, level) & arg.toTree(level)

    if idx != (n.args.len - 1):
      result &= ",\n"

  dec level
  result &= repeat(IndentStr, level) & "]\n"

  dec level
  result &= repeat(IndentStr, level) & ")\n"

func toTree(n: DotExpr, level: var int): string =
  result &= "DotExpr(\n"

  inc level
  result &= repeat(IndentStr, level) & "left: " & n.left.toTree(level) & ",\n"
  result &= repeat(IndentStr, level) & "right: " & n.right.toTree(level) & "\n"
  dec level
  result &= repeat(IndentStr, level) & ")"

func toTree(n: Node, level: var int): string =
  if n of Block:
    result &= Block(n).toTree(level)

  elif n of LiteralExpr:
    result &= LiteralExpr(n).toTree(level)

  elif n of IdentifierExpr:
    result &= IdentifierExpr(n).toTree(level)

  elif n of FunctionCallExpr:
    result &= FunctionCallExpr(n).toTree(level)

  elif n of DotExpr:
    result &= DotExpr(n).toTree(level)

  else:
    raise newException(ZaphytDefect, "Unimplemented node type!")

func toTree*(n: Node): string =
  var indentLevel = 0
  result &= n.toTree(indentLevel)


proc newParser*(tokens: seq[Token]): Parser =
  return Parser(tokens: tokens, pos: 0)

proc parseExpr(p: Parser): Expression =
  result = case p.curToken.typ
    of Identifier:
      IdentifierExpr(ident: p.curToken.value)

    of Int:
      try:
        LiteralExpr(kind: Int64, intVal: parseInt(p.curToken.value))
      except ValueError:
        try:
          LiteralExpr(kind: Uint64, uintVal: parseUint(p.curToken.value))

        except ValueError:
          try:
            LiteralExpr(kind: Float64, floatVal: parseFloat(p.curToken.value))

          except ValueError:
            raise newException(ZaphytParsingError, "Could not parse integer at " &
              &"line {p.curToken.startLine} and column {p.curToken.startColumn}!")

    of Float:
      try:
        LiteralExpr(kind: Float64, floatVal: parseFloat(p.curToken.value))

      except ValueError:
        raise newException(ZaphytParsingError, "Could not parse float at " &
          & "line {p.curToken.startLine} and column {p.curToken.startColumn}!")

    of String:
      LiteralExpr(kind: String, strVal: p.curToken.value.substr(1, p.curToken.value.len - 2))

    of Char:
      LiteralExpr(kind: Char, charVal: p.curToken.value.substr(1, p.curToken.value.len - 2)[0])

    else:
      raise newException(ZaphytParsingError, "Could not parse expression at " &
        & "line {p.curToken.startLine} and column {p.curToken.startColumn}!")

  p.pos += 1

func match(p: Parser, tokenTypes: varargs[TokenType]): bool =
  var cursor = p.pos

  for tokenType in tokenTypes:
    if p.tokens[cursor].typ != tokenType:
      return false

    cursor += 1

  return true

func literalParsingStage(p: Parser): Block =
  result = Block()

  while p.pos < p.tokens.len:
    if p.match(Path, OpenBrace):
      result.children.add FunctionCallExpr(callee: )

    inc p.pos

# TODO: Return a `Module` rather than a `Block`
func parse*(p: Parser): Block =
  result = p.literalParsingStage()