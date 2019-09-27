module Main
import Types
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

MPureFn : Env -> Type
MPureFn env = MalCmd MalVal env (const env)

madd : MalVal -> MalVal -> MalVal
madd (Mv TInt x) (Mv TInt y) = mint (x + y)
madd (Mv t _)    (Mv t' _)   = typeError (wrong TInt t t') TInt

msub : MalVal -> MalVal -> MalVal
msub (Mv TInt x) (Mv TInt y) = mint (x - y)
msub (Mv t x) (Mv t' y) = typeError (wrong TInt t t') TInt

mmul : MalVal -> MalVal -> MalVal
mmul (Mv TInt x) (Mv TInt y) = mint $ x * y
mmul (Mv t x) (Mv t' y) = typeError (wrong TInt t t') TInt


mdiv : MalVal -> MalVal -> MalVal
mdiv (Mv TInt x) (Mv TInt y) with (divides x y)
  mdiv (Mv TInt x) (Mv TInt 0) | DivByZero = divideByZero
  mdiv (Mv TInt ((y * div) + rem)) (Mv TInt y) | (DivBy prf) = mint div
mdiv (Mv t x) (Mv t' y)  = typeError (wrong TInt t t') TInt

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

pureOk : MalVal -> (env : Env) -> MalIO (CmdResult MalVal) env
pureOk v e = pure $ Ok v e

eval' : (env : Env) -> (val : MalVal) -> MalIO (CmdResult MalVal) env
eval' env v@(Mv TInt val) = pureOk v env
eval' env v@(Mv TStr val) = pureOk v env
eval' env v@(Mv TSym name) = let msg = "Symbol " ++ name ++ " unbound." in
                                 case lookup name env of
                                      Nothing => pure $ Error msg
                                      (Just x) => pure (Ok x env)
eval' env v@(Mv TList lst) = case lst of
                                  [] => pure $ Ok v env
                                  (fsym :: args) => case eval' env fsym of
                                                         (Do c f) => do r <- c
                                                                        ?rhs
                                                         (Return val) => pure val
eval' env v@(Mv TFn val) = pure $ Ok v env
eval' env v@(Mv TVec val) = ?rhs_7
eval' env v@(Mv TMap val) = ?rhs_8
eval' env v@(Mv TErr val) = ?rhs_9


eval : Fuel -> Env -> MalVal -> MalVal
eval Dry _ _ = merr "Ran out of fuel - HALTED."
eval _ _ _ = ?rhs
-- eval (More fuel) env form@(Mv TList elems) =
--   case elems of
--        [] => form
--        (head :: args) => case eval fuel env head of
--                               err@(Mv TErr msg) => err
--                               Mv TFn f'   => let args' = map (eval fuel env) args in
--                                                  case find (\x => case x of
--                                                                        (Mv TErr val) => True
--                                                                        (Mv _    val) => False)
--                                                            args' of
--                                                       Nothing => f' args'
--                                                       (Just x) => x
--                               Mv t val    => typeError t TFn

-- eval (More fuel) env (Mv TSym name) with (strList name)
--   eval (More fuel) env (Mv TSym "") | SNil = merr "Internal interpreter error: empty symbol"
--   eval (More fuel) env (Mv TSym (strCons x xs)) | (SCons x rec) = case x of
--                                                                        ':' => msym (strCons x xs)  -- keywords
--                                                                        _ => lookup env (strCons x xs)

-- eval (More fuel) env (Mv TVec elems) = let elems' = map (eval fuel env) elems in
--                                            mvec elems'
-- eval (More fuel) env (Mv TMap elems) = let eval' = eval fuel env
--                                            elems' = map (\(x, y) => (eval' x, eval' y)) elems in
--                                            mmap elems'
-- eval (More fuel) env x = x

partial
rep : Fuel -> String -> String
rep fuel = print . eval fuel env . read

partial
main : IO ()
main = repl "user> " rep'
  where
    partial
    forever : Fuel
    forever = More forever

    partial
    rep' : String -> String
    rep' x = rep forever x ++ "\n"
