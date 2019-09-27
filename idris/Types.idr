module Types

-- mal types
public export
data MalType = TInt
             | TStr
             | TSym
             | TList
             | TFn
             | TVec
             | TMap
             | TErr

public export
Eq MalType where
  (==) TInt TInt = True
  (==) TInt _ = False
  (==) TStr TStr = True
  (==) TStr _ = False
  (==) TSym TSym = True
  (==) TSym _ = False
  (==) TList TList = True
  (==) TList _ = False
  (==) TFn TFn  = True
  (==) TFn _ = False
  (==) TVec TVec  = True
  (==) TVec _ = False
  (==) TMap TMap  = True
  (==) TMap _ = False
  (==) TErr TErr  = True
  (==) TErr _ = False

export
Show MalType where
  show TInt = "Int"
  show TStr = "String"
  show TSym = "Symbol"
  show TList = "List"
  show TFn = "Function"
  show TVec = "Vector"
  show TMap = "Map"
  show TErr = "Error"

mutual
  public export
  repr : MalType -> Type
  repr TErr = String
  repr TInt = Integer
  repr TStr = String
  repr TSym = String
  repr TList = List MalVal
  repr TFn =  MalCmd MalVal ?rhs ?rhs2
  repr TVec = List MalVal
  repr TMap = List (MalVal, MalVal)

  public export
  record MalVal where
    constructor Mv
    type : MalType
    val : repr type


  public export
  Env : Type
  Env = List (String, MalVal)

  assoc : String -> MalVal -> Env -> Env
  assoc name value env = (name, value) :: env

  public export
  data MalCmd : (ty : Type) -> Env -> (ty -> Env) -> Type where
    Bind : (name : String) -> (value : MalVal) -> MalCmd () env (\b => (name, value) :: env)
    Lookup : (name : String) -> MalCmd MalVal env (const env)

    -- TODO : add last-error to the environment?
    Raise : String -> MalCmd MalVal env (const env)

    GetLine : MalCmd String env (const env)
    PutStr : String -> MalCmd () env (const env)

    Pure : ty -> MalCmd ty env (const env)
    (>>=) : MalCmd a env next -> ((x:a) -> MalCmd b (next x) next') -> MalCmd b env next'

  public export
  pure : ty -> MalCmd ty env (const env)
  pure = Pure

  export
  raise : String -> MalCmd MalVal env (const env)
  raise = Raise

  public export
  data MalIO : (ty : Type) -> (env : Env) -> Type where
    Do : MalCmd a env1 env2fn -> ((res : a) -> Inf(MalIO b (env2fn res))) -> MalIO b env1
    Return : ty -> MalIO ty _

  namespace MalIO
    export
    pure : ty -> MalIO ty env
    pure = Return

  namespace MalIODo
    public export
    (>>=) : MalCmd a env1 env2fn -> ((res : a) -> Inf(MalIO b (env2fn res))) -> MalIO b env1
    (>>=) = Do


export
mint : Integer -> MalVal
mint = Mv TInt

export
mstr : String -> MalVal
mstr = Mv TStr

export
msym : String -> MalVal
msym = Mv TSym

export
merr : String -> MalVal
merr = Mv TErr

export
mlist : List MalVal -> MalVal
mlist = Mv TList

--     repr TFn =  (env : Env) -> (next : MalVal -> Env) -> MalCmd MalVal initial next
export
mfun : (List MalVal -> MalVal) -> MalVal
mfun f = Mv TFn ?rhs

export
mmap : List (MalVal, MalVal) -> MalVal
mmap = Mv TMap

export
mvec : List MalVal -> MalVal
mvec = Mv TVec

export
typeError : (actual : MalType) -> (expected : MalType) -> MalVal
typeError actual expected = merr ("Type error: found " ++ show actual ++ "\n" ++
                                  "        expected: " ++ show expected)


export
divideByZero : MalVal
divideByZero = merr "Division by 0"

export
unboundErr : String -> MalVal
unboundErr name = merr $ "Unbound name Error: " ++ name
