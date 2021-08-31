import std/[tables,strutils,hashes]
import pkg/[strfmt]
import ast

type ObjectType* = string

const
 INTEGER_OBJ* = "INTEGER"
 BOOLEAN_OBJ* = "BOOLEAN"
 NULL_OBJ* = "NULL"
 RETURN_VALUE_OBJ* = "RETURN_VALUE"
 ERROR_OBJ* = "ERROR"
 FUNCTION_OBJ* = "FUNCTION"
 STRING_OBJ* = "STRING"
 BUILTIN_OBJ* = "BUILTIN"
 ARRAY_OBJ* = "ARRAY"
 HASH_OBJ* = "HASH"
 
type
  Object* = ref object of RootObj

method Type*(obj: Object): ObjectType {.base.} = ""
method Inspect*(obj: Object): string {.base.} = ""

type
  BuiltinFunction* = proc(args: varargs[Object]): Object
  
type
  Environment* = ref object
    store*: TableRef[string,Object]
    outer*: Environment
    
proc NewEnvironment*(): Environment =
    var s = newTable[string,Object]()
    return Environment(store: s, outer: nil)

proc NewEnclosedEnvironment*(outer: Environment): Environment =
    var env = NewEnvironment()
    env.outer = outer
    return env
        
proc Get*(e: Environment,name: string): (Object, bool) =
    var
      ok = e.store.hasKey(name)
      obj = e.store.getOrDefault(name,nil)
      
    if not ok and e.outer != nil:
       (obj, ok) = e.outer.Get(name)
       
    return (obj,ok)
    
proc Set*(e: Environment,name: string,val: Object): Object =
    e.store[name] = val
    return val
        
type 
  Integer* = ref object of Object
    value*: int64

method Type*(i: Integer): ObjectType = return INTEGER_OBJ
method Inspect*(i: Integer): string = return "{}".fmt($i.value)

type
  Boolean* = ref object of Object
    value*: bool

method Type*(b: Boolean): ObjectType = return BOOLEAN_OBJ
method Inspect*(b: Boolean): string = return "{}".fmt($b.value)

type
  Null* = ref object of Object

method Type*(n: Null): ObjectType = return NULL_OBJ
method Inspect*(n: Null): string = return "null"

type 
  ReturnValue* = ref object of Object
    value*: Object

method Type*(rv: ReturnValue): ObjectType = return RETURN_VALUE_OBJ
method Inspect*(rv: ReturnValue): string = return rv.value.Inspect()

type
  Error* = ref object of Object
   message*: string

method Type*(e: Error): ObjectType = return ERROR_OBJ
method Inspect*(e: Error): string = return "ERROR: " & e.message

type
  Function* = ref object of Object
    parameters*: seq[ast.Identifier]
    body*: ast.BlockStatement
    env*: Environment

method Type*(f: Function): ObjectType = return FUNCTION_OBJ
method Inspect*(f: Function): string = 
     var outs = ""
     var params = newSeq[string]()

     for p in f.parameters:
         params.add(p.String())

     outs &= "fn"
     outs &= "("
     outs &= join(params,", ")
     outs &= ") {\n"
     outs &= f.body.String()
     outs &= "\n}"

     return outs

type 
  String* = ref object of Object
    value*: string

method Type*(s: String): ObjectType = return STRING_OBJ
method Inspect*(s: String): string = return s.value

type
  Builtin* = ref object of Object
    Fn*: BuiltinFunction

method Type*(b: Builtin): ObjectType = return BUILTIN_OBJ
method Inspect*(b: Builtin): string = return "builtin function"

type
  Array* = ref object of Object
    elements*: seq[Object]

method Type*(ao: Array): ObjectType = return ARRAY_OBJ
method Inspect*(ao: Array): string = 
     var outs = ""
     var elems = newSeq[string]()

     for e in ao.elements:
         elems.add(e.Inspect())

     outs &= "["
     outs &= join(elems,", ")
     outs &= "]"

     return outs

proc HashKey*(obj: Object): (hashes.Hash,bool) =
   var h: hashes.Hash = 0

   if obj of Boolean:
      for ch in obj.Inspect():
         h = h !& ord(ch)
      return (!$h, true)
   elif obj of Integer:
      for ch in obj.Inspect():
         h = h !& ord(ch)
      return (!$h, true)
   elif obj of String:
      for ch in obj.Inspect():
         h = h !& ord(ch)
      return (!$h, true)
   else:
      return (-1,false)

type
  HashPair* = ref object
    key*: Object
    value*: Object
    
type
  Hash* = ref object of Object
    pairs*: TableRef[hashes.Hash,HashPair]

method Type*(h: Hash): ObjectType = return HASH_OBJ
method Inspect*(h: Hash): string = 
     var outs = ""
     var pair = newSeq[string]()

     for _,val in h.pairs:
         pair.add(val.key.Inspect() & ":" & val.value.Inspect())

     outs &= "{"
     outs &= join(pair,", ")
     outs &= "}"

     return outs
