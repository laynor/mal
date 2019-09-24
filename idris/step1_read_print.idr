module Main
import Core
import Reader
import Printer

%default total

read : String -> MalType
read x = readString x

print : MalType -> String
print x = printString x

eval : MalType -> MalType
eval x = x

rep : String -> String
rep = print . eval . read

partial
main : IO ()
main = repl "user> " rep'
  where
    rep' : String -> String
    rep' x = rep x ++ "\n"
