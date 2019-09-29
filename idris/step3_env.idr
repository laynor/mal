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


eval : (env1 : Env) -> (val : MalVal) -> MalIO MalVal env1
eval env1 v@(Mv TInt val)  = pure v
eval env1 v@(Mv TStr val)  = pure v
eval env1 v@(Mv TSym name)  = do res <- Lookup name
                                 case res of
                                   Just val => pure val
                                   Nothing => pure $ merr $ "Symbol " ++ name ++ " unbound."

eval env1 v@(Mv TList lst) = case lst of
                                  [] => pure v
                                  (fsym :: args) => do (fsym', env2) <- eval env1 (assert_smaller v fsym)
                                                       case fsym' of
                                                         (Mv TFn f') => do (args', env3) <- evalArgs env2 args
                                                                           f' env3 args'
                                                         (Mv t val) => pure (typeError t TFn)
  where
    evalArgs : (env : Env) -> List MalVal -> MalIO (List MalVal) env
    evalArgs env [] = pure []
    evalArgs env (x :: xs) = do (x', env')   <- eval env x
                                (xs', env'') <- evalArgs env' xs
                                pure ((x' :: xs'))




eval env1 v@(Mv TFn val)  = pure v
eval env1 v@(Mv TVec exprs) = do (vals, env) <- evalExprs env1 exprs
                                 pure (Mv TVec vals)
  where
    evalExprs : (env : Env) -> List MalVal -> MalIO (List MalVal) env
    evalExprs env [] = pure []
    evalExprs env (x :: xs) = do (x', env')   <- eval env x
                                 (xs', env'') <- evalExprs env' xs
                                 pure (x' :: xs')

eval env1 v@(Mv TMap pairs) = do (pairs', env'') <- evalPairs env1 pairs
                                 pure (Mv TMap pairs')
  where
    evalPairs : (env : Env) -> List (MalVal, MalVal) -> MalIO (List (MalVal, MalVal)) env
    evalPairs env [] = pure []
    evalPairs env ((k,v) :: xs) = do (k', env')    <- eval env k
                                     (v', env'')   <- eval env' v
                                     (xs', env''') <- evalPairs env'' xs
                                     pure ((k', v') :: xs')

eval env1 v@(Mv TErr val) = pure v


read : String -> MalVal
read x = readString x

print : MalVal -> String
print x = printString x

partial
rep : Fuel -> Env -> String -> IO(String, Env)
rep fuel env input = let form = read input in
                         do res <- interpret fuel env (eval env form)
                            case res of
                              Ok v env' => pure $ (print v, env')
                              Error err => pure (("ERROR: " ++ err), env)

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
