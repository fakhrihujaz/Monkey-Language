import tokens
import std/[strutils,tables]

type
  Node* = ref object of RootObj
  # Node as interface

type
  Statement* = ref object of Node
  Expression* = ref object of Node

method TokenLiteral*(node: Node): string {.base.} = ""
method String*(node: Node): string {.base.} = ""

type
  Program* = ref object of Node
    statements*: seq[Statement]
    
type
  LetStatement* = ref object of Statement
    token*: tokens.Token
    name*: Identifier
    value*: Expression
  Identifier* = ref object of Expression
    token*: tokens.Token
    value*: string
  ReturnStatement* = ref object of Statement
    token*: tokens.Token
    returnvalue*: Expression
  ExpressionStatement* = ref object of Statement
    token*: tokens.Token
    expression*: Expression
  BlockStatement* = ref object of Statement
    token*: tokens.Token
    statements*: seq[Statement]
  IntegerLiteral* = ref object of Expression
     token*: tokens.Token
     value*: int64
  PrefixExpression* = ref object of Expression
     token*: tokens.Token
     operator*: string
     right*: Expression
  InfixExpression* = ref object of Expression
     token*: tokens.Token
     left*: Expression
     operator*: string
     right*: Expression
  Boolean * = ref object of Expression
     token*: tokens.Token
     value*: bool
  IfExpression* = ref object of Expression
     token*: tokens.Token
     condition*: Expression
     consequence*: BlockStatement
     alternative*: BlockStatement
  FunctionLiteral* = ref object of Expression
     token*: tokens.Token
     parameters*: seq[Identifier]
     body*: BlockStatement
  CallExpression* = ref object of Expression
     token*: tokens.Token
     function*: Expression
     arguments*: seq[Expression]
  StringLiteral* = ref object of Expression
     token*: tokens.Token
     value*: string
  ArrayLiteral* = ref object of Expression
     token*: tokens.Token
     elements*: seq[Expression]
  IndexExpression* = ref object of Expression
     token*: tokens.Token
     left*: Expression
     index*: Expression
  HashLiteral* = ref object of Expression
     token*: tokens.Token
     pairs*: TableRef[Expression,Expression]
     
method TokenLiteral*(p: Program): string =
    if p.statements.len() > 0:
       return p.statements[0].TokenLiteral()
    else:
       return ""

method String*(p: Program): string =
    var buff  = ""

    for s in p.statements:
       buff &= s.String()

    return buff

method TokenLiteral*(ls: LetStatement): string =
    return ls.token.Literal

method String*(ls: LetStatement): string =
    var buff = ""

    buff &= ls.TokenLiteral() & " "

    buff &= ls.name.String()

    buff &= " = "

    if ls.value != nil:
       buff &= ls.value.String()

    buff &= ";"

    return buff

method TokenLiteral*(i: Identifier): string =
    return i.token.Literal

method String*(i: Identifier): string =
    return i.value

method TokenLiteral*(rs: ReturnStatement): string =
    return rs.token.Literal

method String*(rs: ReturnStatement): string =
    var buff = ""

    buff &= rs.TokenLiteral & " "

    if rs.returnvalue != nil:
       buff &= rs.returnvalue.String()

    buff &= ";"

    return buff

method TokenLiteral*(es: ExpressionStatement): string =
    return es.token.Literal

method String*(es: ExpressionStatement): string =
    if es.expression != nil:
       return es.expression.String()
    return ""

method TokenLiteral*(bs: BlockStatement): string =
    return bs.token.Literal

method String*(bs: BlockStatement): string =
    var buff = ""
    for s in bs.statements:
        buff &= s.String()
    return buff
    
method TokenLiteral*(il: IntegerLiteral): string =
    return il.token.Literal

method String*(il: IntegerLiteral): string =
    return il.token.Literal

method TokenLiteral*(pe: PrefixExpression): string =
    return pe.token.Literal

method String*(pe: PrefixExpression): string =
    var buff = ""

    buff &= "("

    buff &= pe.operator

    buff &= pe.right.String()

    buff &= ")"

    return buff

method TokenLiteral*(ie: InfixExpression): string =
    return ie.token.Literal

method String*(ie: InfixExpression): string =
    var buff = " "

    buff &= "("

    buff &= ie.left.String()

    buff &= " " & ie.operator & " "

    buff &= ie.right.String()

    buff &= ")"

    return buff

method TokenLiteral*(b: Boolean): string =
    return b.token.Literal

method String*(b: Boolean): string =
    return b.token.Literal

method TokenLiteral*(ie: IfExpression): string =
    return ie.token.Literal

method String*(ie: IfExpression): string =
    var buff = ""
    buff &= "if"
    buff &= ie.condition.String()
    buff &= " "
    buff &= ie.consequence.String()
    if not ie.alternative.isNil:
       buff &= "else "
       buff &= ie.alternative.String()

    return buff

method TokenLiteral*(fl: FunctionLiteral): string =
      return fl.token.Literal

method String*(fl: FunctionLiteral): string =
      var buff = ""
      var params = newSeq[string]()
      for p in fl.parameters:
          params.add(p.String())
      buff &= fl.TokenLiteral()
      buff &= "("
      buff &= join(params,", ")
      buff &= ")"
      buff &= fl.body.String()

      return buff

method TokenLiteral*(ce: CallExpression): string =
      return ce.token.Literal

method String*(ce: CallExpression): string =
      var buff = ""
      var args= newSeq[string]()
      for a in ce.arguments:
          args.add(a.String())
      buff &= ce.function.String()
      buff &= "("
      buff &= join(args,", ")
      buff &= ")"

      return buff

method TokenLiteral*(sl: StringLiteral): string =
    return sl.token.Literal

method String*(sl: StringLiteral): string =
    return sl.token.Literal

method TokenLiteral*(al: ArrayLiteral): string =
      return al.token.Literal

method String*(al: ArrayLiteral): string =
      var buff = ""
      var elems = newSeq[string]()
      for el in al.elements:
          elems.add(el.String())
      buff &= al.TokenLiteral()
      buff &= "["
      buff &= join(elems,", ")
      buff &= "]"

      return buff

method TokenLiteral*(ie: IndexExpression): string =
    return ie.token.Literal

method String*(ie: IndexExpression): string =
    var buff = ""

    buff &= "("

    buff &= ie.left.String()

    buff &= "["

    buff &= ie.index.String()

    buff &= "])"

    return buff
    
method TokenLiteral*(hl: HashLiteral): string =
      return hl.token.Literal

method String*(hl: HashLiteral): string =
      var buff = ""
      var pair = newSeq[string]()
      for key, value in hl.pairs:
          pair.add(key.String() & ":" & value.String())
          
      buff &= "{"
      buff &= join(pair,", ")
      buff &= "}"

      return buff
