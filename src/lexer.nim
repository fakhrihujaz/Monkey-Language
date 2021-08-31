import tokens

type
  Lexer* = ref object 
    input*: string
    position*: int
    readPosition*: int
    ch*: char

proc New*(input: string): Lexer
proc isSpace(ch: char): bool
proc isLetter(ch: char): bool
proc isDigit(ch: char): bool
proc peekChar(l: Lexer): char
proc readChar(l: Lexer) 
proc readIdentifier(l: Lexer): string
proc readNumber(l: Lexer): string
proc readString(l: Lexer): string
proc skipWhitespace(l: Lexer)
proc NextToken*(l: Lexer): tokens.Token


proc New*(input: string): Lexer =
   let l = Lexer(input: input)
   l.readChar()
   return l

proc readChar(l: Lexer) =
   if l.readPosition >= l.input.len():
      l.ch = '\0'
   else:
      l.ch = l.input[l.readPosition]
   l.position = l.readPosition
   l.readPosition += 1

proc peekChar(l: Lexer): char =
    if l.readPosition >= l.input.len():
       return '\0'
    else:
       return l.input[l.readPosition]
       
proc NextToken*(l: Lexer): tokens.Token =
   var tok = new tokens.Token

   l.skipWhitespace()
   
   case l.ch 
    of '=' : 
       if l.peekChar() == '=':
          var ch = l.ch
          l.readChar()
          tok.Type = tokens.EQ
          tok.Literal = $ch & $l.ch
       else:
          tok.Type = tokens.ASSIGN
          tok.Literal = $l.ch
    of ';' :
       tok.Type = tokens.SEMICOLON
       tok.Literal = $l.ch
    of ':' :
       tok.Type = tokens.COLON
       tok.Literal = $l.ch
    of '(' :
       tok.Type = tokens.LPAREN
       tok.Literal = $l.ch
    of ')' :
       tok.Type = tokens.RPAREN
       tok.Literal = $l.ch
    of ',' :
       tok.Type = tokens.COMMA
       tok.Literal = $l.ch
    of '+' :
       tok.Type = tokens.PLUS
       tok.Literal = $l.ch
    of '-' :
       tok.Type = tokens.MINUS
       tok.Literal = $l.ch
    of '!' :
       if l.peekChar() == '=':
          var ch = l.ch
          l.readChar()
          tok.Type = tokens.NOT_EQ
          tok.Literal = $ch & $l.ch
       else:
          tok.Type = tokens.BANG
          tok.Literal = $l.ch
    of '*' :
       tok.Type = tokens.ASTERISK
       tok.Literal = $l.ch
    of '/' :
       tok.Type = tokens.SLASH
       tok.Literal = $l.ch
    of '<' :
       tok.Type = tokens.LT
       tok.Literal = $l.ch
    of '>' :
       tok.Type = tokens.GT
       tok.Literal = $l.ch
    of '{' :
       tok.Type = tokens.LBRACE
       tok.Literal = $l.ch
    of '}' : 
       tok.Type = tokens.RBRACE
       tok.Literal = $l.ch
    of '[' :
       tok.Type = tokens.LBRACKET
       tok.Literal = $l.ch
    of ']' : 
       tok.Type = tokens.RBRACKET
       tok.Literal = $l.ch
    of '"':
       tok.Type = tokens.STRING
       tok.Literal = l.readString()
    of '\0':
       tok.Type = tokens.EOF
       tok.Literal = ""
    else:
       if l.ch.isLetter():
          tok.Literal = l.readIdentifier()
          tok.Type = tok.Literal.Lookupident()
          return tok
       elif l.ch.isDigit():
          tok.Type = tokens.INT
          tok.Literal = l.readNumber()
          return tok
       else:
          tok.Type = tokens.ILLEGAL
          tok.Literal = $l.ch
 
   l.readChar()
   return tok

const
  letter: set[char] = {'a'..'z','A'..'Z','_'}
  digit : set[char] = {'0'..'9'}
  space : set[char] = {' ','\n','\t','\r'}

proc isSpace(ch: char): bool = return ch in space

proc skipWhitespace(l: Lexer) =
   while l.ch.isSpace():
       l.readChar()
       
proc isLetter(ch: char): bool = return ch in letter
    
proc readIdentifier(l: Lexer): string =
   let position = l.position
   
   while l.ch.isLetter():
       l.readChar()
       
   return l.input[position..<l.position]

proc isDigit(ch: char): bool = return ch in digit

proc readNumber(l: Lexer): string =
   let position = l.position

   while l.ch.isDigit():
       l.readChar()

   return l.input[position..<l.position]

proc readString(l: Lexer): string =
   let position = l.position + 1
   while true:
     l.readChar()
     if l.ch == '"' or l.ch == '\0':
        break

   return l.input[position..<l.position]
