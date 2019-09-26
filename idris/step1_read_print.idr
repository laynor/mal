module Main
import Core
import Reader
import Printer

%default total

read : String -> MalVal
read x = readString x

print : MalVal -> String
print x = printString x

eval : MalVal -> MalVal
eval x = x

rep : String -> String
rep = print . eval . read

partial
main : IO ()
main = repl "user> " rep'
  where
    rep' : String -> String
    rep' x = rep x ++ "\n"
