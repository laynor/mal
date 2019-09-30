module Core

import Types

%default total

public export
typeOf : a -> Type
typeOf x = a

public export
tag : a -> (Type, a)
tag x = (typeOf x, x)

public export
typeOfPrf : tag x = (typeOf x, x)
typeOfPrf = Refl

public export
Show (MalVal -> MalVal) where
  show x = "Function"

interpret' : Env -> MalCmd ty env next -> IO(CmdResult ty)
interpret' env GetEnv = pure $ Ok env env
interpret' env (Raise msg) = pure (Error msg)
interpret' env GetLine = do s <- getLine
                            pure $ Ok s env
interpret' env (PutStr x) = do putStr x
                               pure $ Ok () env
interpret' env (Pure x) = pure (Ok x env)
interpret' env (Let name value) = pure $ Ok () ((name, value) :: env)
interpret' env (Lookup name) = pure (Ok (lookup name env) env)
interpret' env (cmd >>= cont) = do Ok res env' <- interpret' env cmd
                                      | Error err => pure (Error err)
                                   Ok res' env'' <- interpret' env' (cont res)
                                      | Error err => pure (Error err)
                                   pure (Ok res' env'')

public export
data Fuel = Dry | More (Lazy Fuel)

export
interpret  : Fuel -> Env -> MalIO ty env-> IO (CmdResult ty)
interpret  Dry _ _ = pure (Error "Ran out of fuel")
interpret (More fuel) env (Return x) = pure $ Ok x env
interpret  (More fuel ) env (Do cmd cont) = do Ok res env' <- interpret' env cmd
                                                  | Error err => pure (Error err)
                                               Ok res' env'' <- interpret fuel env' (cont res)
                                                  | Error err => pure (Error err)
                                               pure (Ok res' env'')

interpret (More fuel) env (Bind x f) = do x' <- interpret fuel env x
                                          case x' of
                                            (Ok val env') => let y = f val in
                                                                 interpret fuel env' y
                                            (Error x) => pure $ Error x
