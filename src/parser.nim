import tokens,lexer,ast
import pkg/[strfmt]
import std/[tables,strutils,hashes]

const
  LOWEST = 1
  EQUALS = 2
  LESSGREATER = 3
  SUM = 4
  PRODUCT = 5
  PREFIX = 6
  CALL = 7
  INDEX = 8

var
 precedences = {
    tokens.EQ: EQUALS,
    tokens.NOT_EQ: EQUALS,
    tokens.LT: LESSGREATER,
    tokens.GT: LESSGREATER,
    tokens.PLUS: SUM,
    tokens.MINUS: SUM,
    tokens.SLASH: PRODUCT,
    tokens.ASTERISK: PRODUCT,
    tokens.LPAREN: CALL,
    tokens.LBRACKET: INDEX
 }.toTable()
 
type
  prefixParseFn = proc(p: Parser): ast.Expression {.closure.}
  infixParseFn = proc(p: Parser,left: ast.Expression): ast.Expression {.closure.}
  Parser = ref object
   l: lexer.Lexer
   errors: seq[string]
   curToken: tokens.Token
   peekToken: tokens.Token
   prefixParseFns: TableRef[tokens.TokenType,prefixParseFn]
   infixParseFns: TableRef[tokens.TokenType,infixParseFn]
   
proc New*(l: lexer.Lexer): Parser
proc nextToken(p: Parser)
proc ParseProgram*(p: Parser): ast.Program
proc parseStatement(p: Parser): ast.Statement
proc parseLetStatement(p: Parser): ast.LetStatement
proc expectPeek(p: Parser,t: tokens.TokenType): bool
proc curTokenIs(p: Parser,t: tokens.TokenType): bool
proc peekTokenIs(p: Parser,t: tokens.TokenType): bool
proc Errors*(p: Parser): seq[string]
proc peekError(p: Parser,t: tokens.TokenType)
proc parseReturnStatement(p: Parser): ast.ReturnStatement
proc registerPrefix(p: Parser,tokenType: tokens.TokenType,fn: prefixParseFn)
proc registerInfix(p: Parser,tokenType: tokens.TokenType,fn: infixParseFn)
proc parseExpressionStatement(p: Parser): ast.ExpressionStatement
proc parseExpression(p: Parser,precedence: int): ast.Expression
proc parseIdentifier(p: Parser): ast.Expression
proc parseIntegerLiteral(p: Parser): ast.Expression
proc noPrefixParseFnError(p: Parser,t: tokens.TokenType)
proc parsePrefixExpression(p: Parser): ast.Expression
proc peekPrecedence(p: Parser): int
proc curPrecedence(p: Parser): int
proc parseInfixExpression(p: Parser,left: ast.Expression): ast.Expression
proc parseBoolean(p: Parser): ast.Expression
proc parseGroupedExpression(p: Parser): ast.Expression
proc parseBlockStatement(p: Parser): ast.BlockStatement
proc parseIfExpression(p: Parser): ast.Expression
proc parseFunctionLiteral(p: Parser): ast.Expression
proc parseFunctionParameters(p: Parser): seq[ast.Identifier]
proc parseCallExpression(p: Parser, function: ast.Expression): ast.Expression
proc parseStringLiteral(p: Parser): ast.Expression
proc parseArrayLiteral(p: Parser): ast.Expression
proc parseExpressionList(p: Parser, ends: tokens.TokenType): seq[ast.Expression]
proc parseIndexExpression(p: Parser,left: ast.Expression): ast.Expression
proc parseHashLiteral(p: Parser): ast.Expression

proc New*(l: lexer.Lexer): Parser =
    let p = Parser(l: l,errors: @[])

    p.prefixParseFns = newTable[tokens.TokenType,prefixParseFn]()
    p.registerPrefix(tokens.IDENT,parseIdentifier)
    p.registerPrefix(tokens.INT,parseIntegerLiteral)
    p.registerPrefix(tokens.BANG,parsePrefixExpression)
    p.registerPrefix(tokens.MINUS,parsePrefixExpression)
    p.registerPrefix(tokens.TRUE,parseBoolean)
    p.registerPrefix(tokens.FALSE,parseBoolean)
    p.registerPrefix(tokens.LPAREN,parseGroupedExpression)
    p.registerPrefix(tokens.IF,parseIfExpression)
    p.registerPrefix(tokens.FUNCTION,parseFunctionLiteral)
    p.registerPrefix(tokens.STRING,parseStringLiteral)
    p.registerPrefix(tokens.LBRACKET,parseArrayLiteral)
    p.registerPrefix(tokens.LBRACE,parseHashLiteral)
    p.infixParseFns = newTable[tokens.TokenType,infixParseFn]()
    p.registerInfix(tokens.PLUS,parseInfixExpression)
    p.registerInfix(tokens.MINUS,parseInfixExpression)
    p.registerInfix(tokens.SLASH,parseInfixExpression)
    p.registerInfix(tokens.ASTERISK,parseInfixExpression)
    p.registerInfix(tokens.EQ,parseInfixExpression)
    p.registerInfix(tokens.NOT_EQ,parseInfixExpression)
    p.registerInfix(tokens.LT,parseInfixExpression)
    p.registerInfix(tokens.GT,parseInfixExpression)
    p.registerInfix(tokens.LPAREN,parseCallExpression)
    p.registerInfix(tokens.LBRACKET,parseIndexExpression)
    p.nextToken()
    p.nextToken()

    return p

proc nextToken(p: Parser) =
    p.curToken = p.peekToken
    p.peekToken = p.l.NextToken()

proc noPrefixParseFnError(p: Parser,t: tokens.TokenType) =
    var msg = "no prefix parse function for {} found".fmt(t)
    p.errors.add(msg)

proc registerPrefix(p: Parser,tokenType: TokenType,fn: prefixParseFn) =
    p.prefixParseFns[tokenType] = fn

proc registerInfix(p: Parser,tokenType: TokenType,fn: infixParseFn) =
    p.infixParseFns[tokenType] = fn
    
proc ParseProgram*(p: Parser): ast.Program =
    var program = new Program

    program.statements = @[]

    while p.curToken.Type != tokens.EOF:
         var stmt = p.parseStatement()
         if not stmt.isNil:
            program.statements.add(stmt)
         p.nextToken()

    return program

proc parseIdentifier(p: Parser): ast.Expression =
    return Identifier(token: p.curToken, value: p.curToken.Literal)
    
proc parseStringLiteral(p: Parser): ast.Expression =
    return StringLiteral(token: p.curToken, value: p.curToken.Literal)

proc parseIntegerLiteral(p: Parser): ast.Expression =
    var lit = IntegerLiteral(token: p.curToken)
    try:
      let value = parseInt(p.curToken.Literal)
      lit.value = value
      return lit
    except ValueError:
      var msg = "could not parse {} as integer".fmt(p.curToken.Literal)
      p.errors.add(msg)
      return nil

proc parseBoolean(p: Parser): ast.Expression =
   return Boolean(token: p.curToken,value: p.curTokenIs(tokens.TRUE))
   
proc parsePrefixExpression(p: Parser): ast.Expression =
    var expression = PrefixExpression(
          token: p.curToken,
          operator: p.curToken.Literal
    )

    p.nextToken()

    expression.right = p.parseExpression(PREFIX)

    return expression

proc parseInfixExpression(p: Parser,left: ast.Expression): ast.Expression =
     var expression = InfixExpression(
        token: p.curToken,
        operator: p.curToken.Literal,
        left: left
     )

     var precedence = p.curPrecedence()
     
     p.nextToken()
     
     expression.right = p.parseExpression(precedence)

     return expression

proc parseExpression(p: Parser,precedence: int): ast.Expression =
    var prefix = p.prefixParseFns.getOrDefault(p.curToken.Type,nil)
    
    if prefix == nil:
       p.noPrefixParseFnError(p.curToken.Type)
       return nil
    
    var leftExpr = p.prefix()

    while not p.peekTokenIs(tokens.SEMICOLON) and precedence < p.peekPrecedence():
          var infix = p.infixParseFns.getOrDefault(p.peekToken.Type,nil)
          if infix == nil:
             return leftExpr
             
          p.nextToken()
          
          leftExpr = p.infix(leftExpr)
          
    return leftExpr

proc parseGroupedExpression(p: Parser): ast.Expression =
    p.nextToken()
    var exp = p.parseExpression(LOWEST)
    if not p.expectPeek(tokens.RPAREN):
       return nil
    return exp
    
proc hash*(expr: ast.Expression): Hash =
   var ha: Hash = 0
   var str = expr.String()
   for ch in str:
      ha = ha !& ord(ch)
   return !$ha
      
proc parseHashLiteral(p: Parser): ast.Expression =
   var hash = HashLiteral(token: p.curToken)
   hash.pairs = newTable[ast.Expression,ast.Expression]()

   while not p.peekTokenIs(tokens.RBRACE):
         p.nextToken()
         
         let key = p.parseExpression(LOWEST)

         if not p.expectPeek(tokens.COLON):
            return nil
            
         p.nextToken()
         
         let value = p.parseExpression(LOWEST)
         
         hash.pairs[key] = value

         if not p.peekTokenIs(tokens.RBRACE) and not p.expectPeek(tokens.COMMA):
            return nil

   if not p.expectPeek(tokens.RBRACE):
       return nil           
   return hash
         
proc parseArrayLiteral(p: Parser): ast.Expression =
   var Array = ast.ArrayLiteral(token: p.curToken)
   Array.elements = p.parseExpressionList(tokens.RBRACKET)
   return Array
   
proc parseExpressionList(p: Parser, ends: tokens.TokenType): seq[ast.Expression] =
   var list = newSeq[ast.Expression]()

   if p.peekTokenIs(ends):
      p.nextToken()
      return list

   p.nextToken()
   list.add(p.parseExpression(LOWEST))

   while p.peekTokenIs(tokens.COMMA):
         p.nextToken()
         p.nextToken()
         list.add(p.parseExpression(LOWEST))

   if not p.expectPeek(ends):
      return @[]

   return list

proc parseIndexExpression(p: Parser,left: ast.Expression): ast.Expression =
    var exp = IndexExpression(token: p.curToken, left: left)
    p.nextToken()
    exp.index = p.parseExpression(LOWEST)
    if not p.expectPeek(tokens.RBRACKET):
       return nil
    return exp
           
proc parseStatement(p: Parser): ast.Statement =
    case p.curToken.Type
     of tokens.LET :
        return p.parseLetStatement()
     of tokens.RETURN :
        return p.parseReturnStatement()
     else:
        return p.parseExpressionStatement()


proc parseLetStatement(p: Parser): ast.LetStatement =
    var stmt = ast.LetStatement(token: p.curToken)

    if not p.expectPeek(tokens.IDENT):
       return nil

    stmt.name = Identifier(token: p.curToken,
                value: p.curToken.Literal)

    if not p.expectPeek(tokens.ASSIGN):
       return nil
       
    p.nextToken()
    
    stmt.value = p.parseExpression(LOWEST)
    
    while not p.curTokenIs(tokens.SEMICOLON):
         p.nextToken()

    return stmt

proc parseBlockStatement(p: Parser): ast.BlockStatement =
     var blocks = ast.BlockStatement(token: p.curToken)
     blocks.statements = @[]
     p.nextToken()
     while not p.curTokenIs(tokens.RBRACE) and not p.curTokenIs(tokens.EOF):
          var stmt = p.parseStatement()
          if not stmt.isNil:
             blocks.statements.add(stmt)
          p.nextToken()
     return blocks
     
proc parseIfExpression(p: Parser): ast.Expression =
    var expression = ast.IfExpression(token: p.curToken)
    if not p.expectPeek(tokens.LPAREN):
       return nil
    p.nextToken()
    expression.condition = p.parseExpression(LOWEST)
    if not p.expectPeek(tokens.RPAREN):
       return nil
    if not p.expectPeek(tokens.LBRACE):
       return nil
    expression.consequence = p.parseBlockStatement()
    if p.peekTokenIs(tokens.ELSE):
       p.nextToken()
       if not p.expectPeek(tokens.LBRACE):
          return nil
       expression.alternative = p.parseBlockStatement
       
    return expression

proc parseFunctionLiteral(p: Parser): ast.Expression =
    var lit = ast.FunctionLiteral(token: p.curToken)
    
    if not p.expectPeek(tokens.LPAREN):
      return nil
      
    lit.parameters = p.parseFunctionParameters()

    if not p.expectPeek(tokens.LBRACE):
       return nil

    lit.body = p.parseBLockStatement()

    return lit
    
proc parseFunctionParameters(p: Parser): seq[ast.Identifier] =
    var identifiers = newSeq[Identifier]()
    
    if p.peekTokenIs(tokens.RPAREN):
       p.nextToken()
       return identifiers

    p.nextToken()

    var ident = ast.Identifier(token: p.curToken, value: p.curToken.Literal)
    identifiers.add(ident)

    while p.peekTokenIs(tokens.COMMA):
          p.nextToken()
          p.nextToken()
          var ident = ast.Identifier(token: p.curToken, value: p.curToken.Literal)
          identifiers.add(ident)
          
    if not p.expectPeek(tokens.RPAREN):
       return @[]
       
    return identifiers

proc parseCallExpression(p: Parser, function: ast.Expression): ast.Expression =
   var exp = ast.CallExpression(token: p.curToken, function: function)
   exp.arguments = p.parseExpressionList(tokens.RPAREN)
   return exp
   
proc parseReturnStatement(p: Parser): ast.ReturnStatement =
    var stmt = ast.ReturnStatement(token: p.curToken)

    p.nextToken()

    stmt.returnvalue = p.parseExpression(LOWEST)
    
    while p.peekTokenIs(tokens.SEMICOLON):
         p.nextToken()

    return stmt

proc parseExpressionStatement(p: Parser): ast.ExpressionStatement =
    var stmt = ast.ExpressionStatement(token: p.curToken)

    stmt.expression = p.parseExpression(LOWEST)

    if p.peekTokenIs(tokens.SEMICOLON):
       p.nextToken()

    return stmt

proc curTokenIs(p: Parser,t: tokens.TokenType): bool =
    return p.curToken.Type == t

proc peekTokenIs(p: Parser,t: tokens.TokenType): bool =
    return p.peekToken.Type == t

proc expectPeek(p: Parser,t: tokens.TokenType): bool =
     if p.peekTokenIs(t):
        p.nextToken()
        return true
     else:
        p.peekError(t)
        return false

proc peekPrecedence(p: Parser): int =
  if precedences.hasKey(p.peekToken.Type):
     return precedences[p.peekToken.Type]
  else:
     return LOWEST

proc curPrecedence(p: Parser): int =
  if precedences.hasKey(p.curToken.Type):
     return precedences[p.curToken.Type]
  else:
     return LOWEST

proc Errors(p: Parser): seq[string] =
    return p.errors

proc peekError(p: Parser,t: tokens.TokenType) =
   let msg = "expected next token to be {}, got {} instead".fmt(t,p.peekToken.Type)

   p.errors.add(msg)

