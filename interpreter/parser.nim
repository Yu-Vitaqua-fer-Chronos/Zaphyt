import ./[
  lexer
]

type
  Node = ref object of RootObj

  LiteralKind = enum
    Int8, Int16, Int32, Int64, Uint8, Uint16, Uint32, Uint64,
    String, Char, Float32, Float64, Byte

  Literal = ref object of Node
    case kind*: LiteralKind
      of {Int8, Int16, Int32, Int64, Uint8, Uint16, Uint32, Uint64}:
        intVal*: int64

      of {Float32, Float64}:
        floatVal*: float64

      of String:
        strVal*: string

      of Char: # Separate type purely because better imo
        charVal*: char

      of Byte:
        byteVal*: byte
