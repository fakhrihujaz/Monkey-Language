import lexer,parser,evaluator, objects

const
  PROMPT = ">> "
 
proc printParseErrors(errors: seq[string]): void =
    for msg in errors:
      stdout.write "\t" & msg & "\n"
      
proc Start*(): void =
   var quit = false
   var env = objects.NewEnvironment()
   while not quit:
      stdout.write PROMPT
      var Line = stdin.readline()
      quit = Line == "quit"
      if not quit:
         var l = lexer.New(Line)
         var p = parser.New(l)
         var program = p.ParseProgram()
         if len(p.Errors()) != 0:
            printParseErrors(p.Errors())
            continue
         var evaluated = evaluator.eval(program,env)
         if evaluated != nil:
            stdout.write evaluated.Inspect()
            stdout.write "\n"
      
