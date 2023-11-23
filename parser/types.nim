import ../lexer

# Enums
type
  LiteralKind* = enum
    String, Integer, Float, Boolean, Nil

# AST types
type
  Expression* = ref object of RootObj

  Identifier* = ref object of Expression
    name*: string

  FunctionCall* = ref object of Expression
    function*: Expression
    arguments*: seq[Expression]

  Grouping* = ref object of Expression
    expression*: Expression

  Literal* = ref object of Expression
    case kind*: LiteralKind
      of String:
        strVal*: string
      of Integer:
        intVal*: int
      of Float:
        floatVal*: float
      of Boolean:
        boolVal*: bool
      of Nil:
        discard

# Visitor-related code
type
  Visitor* = ref object of RootObj

func visitIdentifier*(v: Visitor, node: Identifier) = discard
func visitFunctionCall*(v: Visitor, node: FunctionCall) = discard
func visitGrouping*(v: Visitor, node: Grouping) = discard
func visitLiteral*(v: Visitor, node: Literal) = discard

func accept*(node: Identifier, v: Visitor) = v.visitIdentifier(node)
func accept*(node: FunctionCall, v: Visitor) = v.visitFunctionCall(node)
func accept*(node: Grouping, v: Visitor) = v.visitGrouping(node)
func accept*(node: Literal, v: Visitor) = v.visitLiteral(node)

# Parser
type
  Parser* = ref object
    tokens*: seq[Token]
    current*: int

func previous(p: Parser): Token = p.tokens[p.current - 1]
func peek(p: Parser): Token = p.tokens[p.current]
func atEnd(p: Parser): bool = p.peek().typ == EOF

func check(p: Parser, typ: TokenType): bool =
  if p.atEnd:
    return false
  else:
    return p.peek().typ == typ

func advance(p: Parser) =
  if not p.atEnd:
    p.current += 1

func match(p: Parser, types: set[TokenType]): bool =
  for typ in types:
    if p.check(typ):
      p.advance()
      return true

  return false

func expression(p: Parser): Expression

func primary(p: Parser): Expression =
  if p.peek.value == "true":
    p.advance()
    return Literal(kind: Boolean, boolVal: true)

  elif p.peek.value == "false":
    p.advance()
    return Literal(kind: Boolean, boolVal: false)

  elif p.peek.value == "nil":
    p.advance()
    return Literal(kind: Nil)

  elif p.match({TokenType.String, TokenType.Char, Int, TokenType.Float}):
    if p.previous.typ == TokenType.String:
      return Literal(kind: String, strVal: p.previous.value)

    elif p.previous.typ == TokenType.Char:
      return Literal(kind: String, strVal: p.previous.value)

    elif p.previous.typ == Int:
      return Literal(kind: Integer, intVal: parseInt(p.previous.value))

    elif p.previous.typ == TokenType.Float:
      return Literal(kind: Float, floatVal: parseFloat(p.previous.value))

func unary(p: Parser): Expression =
  if p.peek.value in ["not", "-"]:
    p.advance()

    let
      op = p.previous
      right = p.unary()

    return FunctionCall(function: Identifier(name: op.value), arguments: @[right])

  return p.primary()

func factor(p: Parser): Expression =
  result = p.unary()

  while p.peek.value in ["*", "/", "%"]:
    p.advance()

    let
      op = p.previous
      right = p.unary()

    result = FunctionCall(function: Identifier(name: op.value), arguments: @[result, right])

func term(p: Parser): Expression =
  result = p.factor()

  while p.peek.value in ["+", "-"]:
    p.advance()

    let
      op = p.previous
      right = p.factor()

    result = FunctionCall(function: Identifier(name: op.value), arguments: @[result, right])

func comparison(p: Parser): Expression =
  result = p.term()

  while p.peek.value in ["<", ">", "<=", ">="]:
    p.advance()

    let
      op = p.previous
      right = p.term()

    result = FunctionCall(function: Identifier(name: op.value), arguments: @[result, right])

func equality(p: Parser): Expression =
  result = p.comparison()

  while p.peek.value in ["==", "!="]:
    p.advance()

    let
      op = p.previous
      right = p.comparison()

    result = FunctionCall(function: Identifier(name: op.value), arguments: @[right])

func expression(p: Parser): Expression = p.equality()