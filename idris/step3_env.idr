module Main
import Types
import Core
import Reader
import Printer
import Data.Vect
import Data.Primitives.Views
import Data.String.Views

%default total

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
env = [ ("+",  mfun (\env, args => pure $ foldr madd (mint 0) args)),

        ("-", mfun (\env, args => pure $ case args of
                                                [] => merr "Not enough arguments."
                                                (Mv TInt x) :: xs => foldl msub (mint x) xs
                                                _ => merr "Type error")),

        ("*", mfun (\env, args => pure $ foldr mmul (mint 1) args)),

        ("/", mfun (\env, args => pure $ case args of
                                                [] => merr "Not enough arguments."
                                                (Mv TInt x) :: xs => foldl mdiv (mint x) xs
                                                _ => merr "Type error")),
        ("set", mfun (\env, args => do Let "foo" (mint 12)
                                       Return (msym "foo")))
        ]

pureOk : MalVal -> (env : Env) -> MalIO (CmdResult MalVal) env
pureOk v e = pure $ Ok v e


eval :(val : MalVal) -> MalIO MalVal env1
eval v@(Mv TInt val)  = pure v
eval v@(Mv TStr val)  = pure v
eval v@(Mv TSym name)  = do res <- Lookup name
                            case res of
                                 Just val => pure val
                                 Nothing => pure $ merr $ "Symbol " ++ name ++ " unbound."

eval v@(Mv TList lst) = case lst of
                             [] => pure v
                             (fsym :: args) => do env1  <- GetEnv
                                                  ?rhs
                                                  -- case fsym' of
                                                  --   (Mv TFn f') => do args' <- evalArgs args
                                                  --                     ?rhs
                                                  --                     -- pure (mint 2)
                                                  --                     -- f' args'
                                                  --   (Mv t val) => pure (typeError t TFn)
  where
    evalArgs : List MalVal -> MalIO (List MalVal) env12
    evalArgs [] = pure []
    evalArgs (x :: xs) = do x'  <- eval x
                            xs' <- evalArgs xs
                            pure (x' :: xs')




eval v@(Mv TFn val)  = pure v
eval v@(Mv TVec exprs) = do vals <- evalExprs exprs
                            ?rhs
                            -- pure (Mv TVec vals)
  where
    evalExprs : List MalVal -> MalIO (List MalVal) env12
    evalExprs [] = pure []
    evalExprs (x :: xs) = do x'  <- eval x
                             ?rhs
                             -- xs' <- evalExprs xs
                             -- pure (x' :: xs')

eval v@(Mv TMap pairs) = do pairs' <- evalPairs pairs
                            ?rhs
                            -- pure (Mv TMap pairs')
  where
    evalPairs : List (MalVal, MalVal) -> MalIO (List (MalVal, MalVal)) env12
    evalPairs [] = pure []
    evalPairs ((k,v) :: xs) = do k'  <- eval k
                                 ?rhs
                                 -- v'  <- eval v
                                 -- xs' <- evalPairs xs
                                 -- pure ((k', v') :: xs')

eval v@(Mv TErr val) = pure v


read : String -> MalVal
read x = readString x

print : MalVal -> String
print x = printString x

partial
rep : Fuel -> Env -> String -> IO(String, Env)
rep fuel env input = let form = read input in
                         do res <- interpret fuel env (eval form)
                            ?rhs
                            -- case res of
                            --   Ok v env' => pure $ (print v, env')
                            --   Error err => pure (("ERROR: " ++ err), env)

partial
main : IO ()
main = malRepl "user> " env
  where
    partial
    forever : Fuel
    forever = More forever

    partial
    malRepl : String -> Env -> IO()
    malRepl prompt  env = do putStr prompt
                             input <- getLine
                             case input of
                               "(exit)" => putStrLn "Bye"
                               _ => do (res, env') <- rep forever env input
                                       putStrLn res
                                       malRepl prompt env'
