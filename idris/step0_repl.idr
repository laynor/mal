module Main

read : String -> String
read x = x

print : String -> String
print x = x

eval : String -> String
eval x = x

rep : String -> String
rep = print . eval . read

main : IO ()
main = repl "user> " rep'
  where
    rep' : String -> String
    rep' x = rep x ++ "\n"
