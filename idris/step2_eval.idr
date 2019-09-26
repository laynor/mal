module Main
import Core
import Reader
import Printer
import Data.Vect
import Data.Primitives.Views
import Data.String.Views

%default total


read : String -> MalVal
read x = readString x

print : MalVal -> String
print x = printString x

atomp : MalVal -> Bool
atomp (Mv TList val) = True
atomp _ = False

wrong : MalType -> MalType -> MalType -> MalType
wrong expected t1 t2 = if t1 == expected then t2 else t1

madd : MalVal -> MalVal -> MalVal
madd (Mv TInt x) (Mv TInt y) = mint $ x + y
madd (Mv t _)    (Mv t' _)   = typeError (wrong TInt t t') TInt

msub : MalVal -> MalVal -> MalVal
msub (Mv TInt x) (Mv TInt y) = mint $ x - y
msub (Mv t x) (Mv t' y) = typeError (wrong TInt t t') TInt

mmul : MalVal -> MalVal -> MalVal
mmul (Mv TInt x) (Mv TInt y) = mint $ x * y
mmul (Mv t x) (Mv t' y) = typeError (wrong TInt t t') TInt


mdiv : MalVal -> MalVal -> MalVal
mdiv (Mv TInt x) (Mv TInt y) with (divides x y)
  mdiv (Mv TInt x) (Mv TInt 0) | DivByZero = divideByZero
  mdiv (Mv TInt ((y * div) + rem)) (Mv TInt y) | (DivBy prf) = mint div
mdiv (Mv t x) (Mv t' y)  = typeError (wrong TInt t t') TInt

Env : Type
Env = List (String, MalVal)

env : Env
env = [ ("+",  mfun (\args => foldr madd (mint 0) args)),

        ("-", mfun (\args => case args of
                                  [] => merr "Not enough arguments."
                                  (Mv TInt x) :: xs => foldl msub (mint x) xs
                                  _ => merr "Type error")),

        ("*", mfun (\args => foldr mmul (mint 1) args)),

        ("/", mfun (\args => case args of
                                  [] => merr "Not enough arguments."
                                  (Mv TInt x) :: xs => foldl mdiv (mint x) xs
                                  _ => merr "Type error"))
        ]

lookup : Env -> String -> MalVal
lookup env key = case find ((==key) . fst) env of
                      Nothing => unboundErr key
                      Just (k, v) => v

data Fuel = Dry | More (Lazy Fuel)


eval : Fuel -> Env -> MalVal -> MalVal
eval Dry _ _ = merr "Ran out of fuel - HALTED."
eval (More fuel) env form@(Mv TList elems) =
  case elems of
       [] => form
       (head :: args) => case eval fuel env head of
                              err@(Mv TErr msg) => err
                              Mv TFn f'   => let args' = map (eval fuel env) args in
                                                 case find (\x => case x of
                                                                       (Mv TErr val) => True
                                                                       (Mv _    val) => False)
                                                           args' of
                                                      Nothing => f' args'
                                                      (Just x) => x
                              Mv t val    => typeError t TFn

eval (More fuel) env (Mv TSym name) with (strList name)
  eval (More fuel) env (Mv TSym "") | SNil = merr "Internal interpreter error: empty symbol"
  eval (More fuel) env (Mv TSym (strCons x xs)) | (SCons x rec) = case x of
                                                                       ':' => msym (strCons x xs)  -- keywords
                                                                       _ => lookup env (strCons x xs)

eval (More fuel) env (Mv TVec elems) = let elems' = map (eval fuel env) elems in
                                           mvec elems'
eval (More fuel) env (Mv TMap elems) = let eval' = eval fuel env
                                           elems' = map (\(x, y) => (eval' x, eval' y)) elems in
                                           mmap elems'
eval (More fuel) env x = x

partial
rep : Fuel -> String -> String
rep fuel = print . eval fuel env . read

partial
forever : Fuel
forever = More forever

partial
main : IO ()
main = repl "user> " rep'
  where
    partial
    rep' : String -> String
    rep' x = rep forever x ++ "\n"
