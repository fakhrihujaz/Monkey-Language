import std/[tables]

type TokenType* = string

type
  Token* = ref object 
    Type*: TokenType
    Literal*: string
    
const
  ILLEGAL* = "ILLEGAL"
  EOF* = "EOF"
  
  IDENT* = "IDENT" # foo, bar
  INT* = "INT" # 123
  STRING* = "STRING"
  
  ASSIGN* = "=" # =
  PLUS* = "+" # +
  MINUS* = "-" # -
  BANG* = "!" # !
  ASTERISK* = "*" # *
  SLASH* = "/" # /

  LT* = "<" # <
  GT* = ">" # >

  EQ* = "=="
  NOT_EQ* = "!="
  
  COMMA* = "," # ,
  SEMICOLON* = ";" # ;
  COLON* = ":"
  LPAREN* = "(" # (
  RPAREN* = ")" # )
  LBRACE* = "{" # {
  RBRACE* = "}" # }
  LBRACKET* = "["
  RBRACKET* = "]"
  FUNCTION* = "FUNCTION"
  LET* = "LET"
  TRUE* = "TRUE"
  FALSE* = "FALSE"
  IF* = "IF"
  ELSE* = "ELSE"
  RETURN* = "RETURN"

var
 keywords = {
    "fn" : FUNCTION,
    "let": LET,
    "true": TRUE,
    "false": FALSE,
    "if" : IF,
    "else": ELSE,
    "return": RETURN
 }.newTable

proc LookupIdent*(ident: string): TokenType =
    if keywords.hasKey(ident):
       return keywords[ident]
    else:
       return IDENT
