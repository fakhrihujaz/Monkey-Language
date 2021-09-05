import std/[osproc,strutils]
import pkg/[strfmt]
import repl

if isMainModule:
   let user = osproc.execProcess("echo $USER").split()[0]
   stdout.write "Hello {} this is the Monkey programming language\n".fmt($user)
   stdout.write "Feel free to type in commands\n"
   repl.Start()
   
