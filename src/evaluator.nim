import ast,objects
import std/[strformat,tables,hashes]

proc eval*(node: ast.Node,env: objects.Environment): objects.Object
proc evalProgram(program: ast.Program,env: objects.Environment): objects.Object
proc evalBlockStatement(blocks: ast.BlockStatement,env: objects.Environment): objects.Object
proc nativeBoolToBooleanObject(input: bool): objects.Boolean
proc evalPrefixExpression(operator: string,right: objects.Object): objects.Object
proc evalBangOperatorExpression(right: objects.Object): objects.Object
proc evalMinusPrefixOperatorExpression(right: objects.Object): objects.Object
proc evalInfixExpression(operator: string,left,right: objects.Object): objects.Object
proc evalIntegerInfixExpression(operator: string,left,right: objects.Object): objects.Object
proc evalIfExpression(ie: ast.IfExpression,env: objects.Environment): objects.Object
proc newError(format: string): objects.Error
proc isError(obj: objects.Object): bool
proc evalIdentifier(node: ast.Identifier, env: objects.Environment): objects.Object
proc evalExpression(exp: seq[ast.Expression],env: Environment): seq[objects.Object]
proc applyFunction(fn: objects.Object, args: seq[objects.Object]): objects.Object
proc evalStringInfixExpression(operator: string,left,right: objects.Object): objects.Object
proc evalIndexExpression(left,index: objects.Object): objects.Object
proc evalArrayIndexExpression(arr,index: objects.Object): objects.Object
proc evalHashLiteral(node: ast.HashLiteral,env: objects.Environment): objects.Object
proc evalHashIndexExpression(hashh,index: objects.Object): objects.Object

var
  NULL = objects.Null()
  TRUE = objects.Boolean(value: true)
  FALSE = objects.Boolean(value: false)

var builtins = {
    "len": objects.Builtin(
        Fn: proc(args: varargs[objects.Object]): objects.Object =
                 if len(args) != 1:
                    return newError(fmt"wrong number of arguments. got={len(args)}, want=1")

                 if args[0] of objects.Array:
                    var arg = objects.Array(args[0])
                    return objects.Integer(value: int64(len(arg.elements)))
                 elif args[0] of objects.String:
                   var arg = objects.String(args[0])
                   return objects.Integer(value: int64(len(arg.value)))
                 else:
                   return newError(fmt"arguments to `len` not supported, got {args[0].Type()}")
    ),
   "first": objects.Builtin(
        Fn: proc(args: varargs[objects.Object]): objects.Object =
                 if len(args) != 1:
                    return newError(fmt"wrong number of arguments. got={len(args)}, want=1")

                 if args[0].Type() != objects.ARRAY_OBJ:
                    return newError(fmt"argument to `first` must be array, got {args[0].Type()}")
                 var arr = objects.Array(args[0])
                 if len(arr.elements) > 0:
                    return arr.elements[0]
                 return NULL
   ),
  "last": objects.Builtin(
       Fn: proc(args: varargs[objects.Object]): objects.Object =
                if len(args) != 1:
                   return newError(fmt"wrong number of arguments. got={len(args)}, want=1")

                if args[0].Type() != objects.ARRAY_OBJ:
                   return newError(fmt"argument to `last` must be array, got {args[0].Type()}")
                var arr = objects.Array(args[0])
                if len(arr.elements) > 0:
                   return arr.elements[^1]
                return NULL
   ),
  "rest": objects.Builtin(
       Fn: proc(args: varargs[objects.Object]): objects.Object =
                if len(args) != 1:
                   return newError(fmt"wrong number of arguments. got={len(args)}, want=1")
  
                if args[0].Type() != objects.ARRAY_OBJ:
                   return newError(fmt"argument to `rest` must be array, got {args[0].Type()}")
                var arr = objects.Array(args[0])
                var length = len(arr.elements)
                if len(arr.elements) > 0:
                   var newElements = newSeq[objects.Object](length - 1)
                   newElements = arr.elements[1..^1]
                   return objects.Array(elements: newElements)
                return NULL
  ),
 "push": objects.Builtin(
      Fn: proc(args: varargs[objects.Object]): objects.Object =
               if len(args) != 2:
                  return newError(fmt"wrong number of arguments. got={len(args)}, want=2")
  
               if args[0].Type() != objects.ARRAY_OBJ:
                  return newError(fmt"argument to `push` must be array, got {args[0].Type()}")
               var arr = objects.Array(args[0])
               var length = len(arr.elements)
               var newElements = newSeq[objects.Object](length + 1)
               newElements = arr.elements
               newElements.add(args[1])
               return objects.Array(elements: newElements)
  ),
 "puts": objects.Builtin(
      Fn: proc(args: varargs[objects.Object]): objects.Object =
               for arg in args:
                   echo $arg.Inspect()
                   
               discard NULL
 )
}.newTable()

proc eval*(node: ast.Node, env: objects.Environment): objects.Object =

  if node of ast.Program:
     var program =  ast.Program(node)
     return evalProgram(program,env)
     
  elif node of ExpressionStatement:
     var expr = ast.ExpressionStatement(node)
     return eval(expr.expression,env)

  elif node of IntegerLiteral:
     var Int = ast.IntegerLiteral(node)
     return objects.Integer(value: Int.value)

  elif node of ast.Boolean:
     var Bool = ast.Boolean(node)
     return nativeBoolToBooleanObject(Bool.value)

  elif node of ast.PrefixExpression:
     var prefix = ast.PrefixExpression(node)
     var right = eval(prefix.right,env)
     if isError(right):
        return right
     return evalPrefixExpression(prefix.operator,right)

  elif node of ast.InfixExpression:
     var infix = ast.InfixExpression(node)
     var left = eval(infix.left,env)
     if isError(left):
        return left
     var right = eval(infix.right,env)
     if isError(right):
        return right
     return evalInfixExpression(infix.operator,left,right)

  elif node of ast.BlockStatement:
    var blocks = ast.BlockStatement(node)
    return evalBlockStatement(blocks,env)
    
  elif node of ast.IfExpression:
    var ifexpr = ast.IfExpression(node)
    return evalIfExpression(ifExpr,env)

  elif node of ast.ReturnStatement:
    var rs = ast.ReturnStatement(node)
    var val = eval(rs.returnvalue,env)
    if isError(val):
       return val
    return objects.ReturnValue(value: val)

  elif node of ast.LetStatement:
     var lets = ast.LetStatement(node)
     var val = eval(lets.value,env)
     if isError(val):
        return val
     discard env.Set(lets.name.value,val)
     
  elif node of ast.Identifier:
     var ident = ast.Identifier(node)
     return evalIdentifier(ident,env)
  elif node of ast.FunctionLiteral:
     var fn = ast.FunctionLiteral(node)
     var params = fn.parameters
     var body = fn.body
     return Function(parameters: params, body: body, env: env)
  elif node of ast.CallExpression:
     var call = ast.CallExpression(node)
     var function = eval(call.function,env)
     if isError(function):
        return function
     var args = evalExpression(call.arguments,env)
     if len(args) == 1 and isError(args[0]):
        return args[0]
     return applyFunction(function,args)
  elif node of ast.StringLiteral:
     var str = ast.StringLiteral(node)
     return objects.String(value: str.value)
  elif node of ast.ArrayLiteral:
     var arr = ast.ArrayLiteral(node)
     var elems = evalExpression(arr.elements,env)
     if len(elems) == 1 and isError(elems[0]):
        return elems[0]
     return objects.Array(elements: elems)
  elif node of ast.IndexExpression:
     var idx = ast.IndexExpression(node)
     var left = eval(idx.left,env)
     if isError(left):
        return left
     var index = eval(idx.index,env)
     if isError(index):
        return index
     return evalIndexExpression(left,index)
  elif node of ast.HashLiteral:
       var hs = ast.HashLiteral(node)
       return evalHashLiteral(hs,env)
  else:     
     return nil

proc evalHashLiteral(node: ast.HashLiteral,env: objects.Environment): objects.Object =
    var pairs = newTable[hashes.Hash,objects.HashPair]()
    for keyNode, valueNode in node.pairs:
        var key = eval(keyNode,env)
        if isError(key):
           return key
        var (hashKey,ok) = HashKey(key)
        if not ok and hashKey == -1:
           return newError("unusable as hash key: " & key.Type())

        var value = eval(valueNode,env)
        if isError(value):
           return value

        var hashed = hashKey
        pairs[hashed] = objects.HashPair(key: key, value: value)
    return objects.Hash(pairs: pairs)

proc evalHashIndexExpression(hashh,index: objects.Object): objects.Object =
    var hashObject = objects.Hash(hashh)
    var (key,ok) = HashKey(index)
    if not ok and key == -1:
       return newError(fmt"unusable as hash key: " & index.Type())
    var pair = hashObject.pairs.getOrDefault(key,nil)
    if pair == nil:
       return NULL
    return pair.value
    
proc evalExpression(exp: seq[ast.Expression],env: Environment): seq[objects.Object] =
    var results = newSeq[objects.Object]()
    for e in exp:
       var evaluated = eval(e,env)
       if isError(evaluated):
          return @[evaluated]
       results.add(evaluated)
    return results
    
proc evalProgram(program: ast.Program,env: objects.Environment): objects.Object =
    var results = objects.Object()

    for statement in program.statements:
        results = eval(statement,env)

        if results of objects.ReturnValue:
           var returnvalue = objects.ReturnValue(results)
           return returnvalue.value
        elif results of objects.Error:
           var err = objects.Error(results)
           return err
           
    return results
    
proc evalPrefixExpression(operator: string,right: objects.Object): objects.Object =
    case operator:
     of "!": 
        return evalBangOperatorEXpression(right)
     of "-":
        return evalMinusPrefixOperatorExpression(right)
     else:
        return newError(fmt"unknown operator: {operator}{right.Type()}")

        
proc evalBangOperatorExpression(right: objects.Object): objects.Object =
     if right == TRUE:
        return FALSE
     elif right == FALSE:
        return TRUE
     elif right == NULL:
        return TRUE
     else:
        return FALSE
        
proc evalMinusPrefixOperatorExpression(right: objects.Object): objects.Object =
     if right.Type() != objects.INTEGER_OBJ:
        return newError(fmt"unknown operator: -{right.Type()}")

     var value = objects.Integer(right).value
     return objects.Integer(value: -value)

proc evalInfixExpression(operator: string,left,right: objects.Object): objects.Object =
     if left.Type() == objects.INTEGER_OBJ and right.Type() == objects.INTEGER_OBJ:
        return evalIntegerInfixExpression(operator,left,right)
     elif left.Type() == objects.STRING_OBJ and right.Type() == objects.STRING_OBJ:
          return evalStringInfixExpression(operator,left,right)
     elif operator == "==":
        return nativeBoolToBooleanObject(left == right)
     elif operator == "!=":
        return nativeBoolToBooleanObject(left != right)
     elif left.Type() != right.Type():
        return newError(fmt"type mismatch: {left.Type()} {operator} {right.Type()}")
     else:
       return newError(fmt"unknown operator: {left.Type()} {operator} {right.Type()}")
        
proc evalIntegerInfixExpression(operator: string,left,right: objects.Object): objects.Object =
     var leftval = objects.Integer(left).value
     var rightval = objects.Integer(right).value

     case operator
      of "+":
        return objects.Integer(value: leftval + rightval) 
      of "-":
        return objects.Integer(value: leftval - rightval) 
      of "*":
        return objects.Integer(value: leftval * rightval) 
      of "/":
        return objects.Integer(value: leftval div rightval) 
      of "<":
        return nativeBoolToBooleanObject(leftval < rightval)
      of ">":
        return nativeBoolToBooleanObject(leftval > rightval)
      of "==":
        return nativeBoolToBooleanObject(leftval == rightval)
      of "!=":
        return nativeBoolToBooleanObject(leftval != rightval)
      else:    
        return newError(fmt"unknown operator: {left.Type()} {operator} {right.Type()}")

proc evalStringInfixExpression(operator: string,left,right: objects.Object): objects.Object =
    if operator != "+":
       return newError(fmt"unknown operator: {left.Type()} {operator} {right.Type()}")
    var
      leftval = objects.String(left).value
      rightval = objects.String(right).value
    return objects.String(value: leftval & rightval)
        
proc isTruthy(obj: objects.Object): bool =
    if obj == NULL:
       return false
       
    elif obj == TRUE:
       return true
       
    elif obj == FALSE:
       return false
       
    else:   
       return true
    
proc evalIfExpression(ie: ast.IfExpression,env: objects.Environment): objects.Object =
    var condition = eval(ie.condition,env)
    if isError(condition):
       return condition
    if isTruthy(condition):
      return eval(ie.consequence,env)
    elif ie.alternative != nil:
      return eval(ie.alternative,env)
    else:
      return NULL
              
proc evalBlockStatement(blocks: ast.BlockStatement,env: objects.Environment): objects.Object =
   var results = objects.Object()

   for statement in blocks.statements:
      results = eval(statement,env)

      if results != nil and results.Type() == objects.RETURN_VALUE_OBJ:
         return results

   return results

proc evalIndexExpression(left,index: objects.Object): objects.Object =
   if left.Type() == objects.ARRAY_OBJ and index.Type() == objects.INTEGER_OBJ:
      return evalArrayIndexExpression(left, index)
   elif left.Type() == objects.HASH_OBJ:
      return evalHashIndexExpression(left, index)
   return newError(fmt"index operator not supported: {left.Type()}")
   
proc evalArrayIndexExpression(arr,index: objects.Object): objects.Object =
    var arrayObject = objects.Array(arr)
    var idx = objects.Integer(index).value
    var max = int64(len(arrayObject.elements) - 1)
    if idx < 0 or idx > max:
       return NULL
    return arrayObject.elements[idx]
    
proc nativeBoolToBooleanObject(input: bool): objects.Boolean =
    if input:
       return TRUE
    return FALSE

proc newError(format: string): objects.Error =
   return objects.Error(
      message: format
   )

proc isError(obj: objects.Object): bool =
   if obj != nil:
      return obj.Type() == objects.ERROR_OBJ
   return false

proc evalIdentifier(node: ast.Identifier, env: objects.Environment): objects.Object =
    var (val, ok) = env.Get(node.value)
    if ok: return val
    var 
      found = builtins.hasKey(node.value)
      builtin = builtins.getOrDefault(node.value,nil)
    if found: return builtin
    return newError("identifier not found " & node.value)

proc extendedFunctionEnv(fn: objects.Function, args: seq[objects.Object]): objects.Environment =
    var env = objects.NewEnclosedEnvironment(fn.env)
    for paramidx, param in fn.parameters:
        discard env.Set(param.value, args[paramIdx])
    return env

proc unwrapReturnValue(obj: objects.Object): objects.Object =
    if obj of objects.ReturnValue:
       var returnvalue = objects.ReturnValue(obj)
       return returnvalue.value
    return obj
    
proc applyFunction(fn: objects.Object, args: seq[objects.Object]): objects.Object =
    if fn of objects.Function:
         var function = objects.Function(fn)
         var extendedEnv = extendedFunctionEnv(function, args)
         var evaluated = eval(function.body,extendedEnv)
         return unwrapReturnValue(evaluated)
    elif fn of objects.Builtin:
         var built = objects.Builtin(fn)
         return built.Fn(args)
    return newError(fmt"not a function: {fn.Type()}")
