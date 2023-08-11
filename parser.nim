import ./[
  lexer
]

type
  Node* = ref object of RootObj

  RootNode* = ref object of Node
    body*: seq[Node]

  LiteralKind* = enum
    Int8, Int16, Int32, Int64, Uint8, Uint16, Uint32, Uint64,
    String, Char, Float32, Float64, Byte

  Literal* = ref object of Node
    case kind*: LiteralKind
      of {Int8, Int16, Int32, Int64, Uint8, Uint16, Uint32, Uint64}:
        intVal*: int64

      of {Float32, Float64}:
        floatVal*: float64

      of Byte:
        byteVal*: byte

      of Char:
        charVal*: char

      of String:
        strVal*: string

  Identifier* = ref object of Node
    name*: string

  Block* = ref object of Node
    body*: seq[Node]


proc parse*(tokens: seq[Token]): RootNode =
  result = RootNode()

