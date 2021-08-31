import std/[osproc]
import pkg/[strfmt]
import repl

if isMainModule:
   let user = osproc.execProcess("echo $USER")
   stdout.write "Hello {} this is the Monkey programming language\n".fmt($user)
   stdout.write "Feel free to type in commands\n"
   repl.Start()
   
