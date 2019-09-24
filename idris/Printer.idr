module Printer
import Core

%default total

public export
spaceSeparated : List String -> String
spaceSeparated xs = trim $ foldr (\x, acc => x ++ " " ++ acc) "" xs

public export
delimited : String -> List String -> String -> String
delimited op xs cl = let contents = spaceSeparated xs in
                         op ++ contents ++ cl


public export total
printString : MalType -> String
printString (MError x) = x
printString (MInt x) = show x
printString (MStr x) = show x
printString (MSym x) = x
printString MNil = "()"
printString (MList xs) = let elems = (map (\x => printString (assert_smaller (MList xs) x)) xs) in
                             delimited "(" elems ")"
printString (MVec xs) = let elems = assert_total (map printString xs) in
                            delimited "["  elems  "]"
printString (MMap xs) = let elems = assert_total (map (\(k, v) => printString k ++ " " ++ printString v)
                                                      xs)  in
                            delimited "{" elems "}"
