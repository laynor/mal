module Printer
import Types
import Core

%default total

export
spaceSeparated : List String -> String
spaceSeparated xs = trim $ foldr (\x, acc => x ++ " " ++ acc) "" xs



export
delimited : String -> List String -> String -> String
delimited op xs cl = let contents = spaceSeparated xs in
                         op ++ contents ++ cl


export
Show MalVal where
  show (Mv TInt x) = show x
  show (Mv TStr x) = show x
  show (Mv TSym x) = x
  show (Mv TList x) = let elems = assert_total $ map show x in
                          "(" ++ spaceSeparated elems ++ ")"
  show (Mv TFn x) = "<Function>"
  show (Mv TVec x) = let elems = assert_total $ map show x in
                         "[" ++ spaceSeparated elems ++ "]"

  show (Mv TMap x) = let elems = assert_total $ map (\(k,v) => show k ++ " " ++ show v) x in
                         "{" ++ spaceSeparated elems ++ "}"
  show (Mv TErr x) = x


export
printString : MalVal -> String
printString = show
