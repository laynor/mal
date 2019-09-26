module Core

%default total

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

-- i valori possono essere di un certo tipo

public export
typeOf : a -> Type
typeOf x = a

public export
tag : a -> (Type, a)
tag x = (typeOf x, x)

public export
typeOfPrf : tag x = (typeOf x, x)
typeOfPrf = Refl

mutual
  public export
  repr : MalType -> Type
  repr TErr = String
  repr TInt = Integer
  repr TStr = String
  repr TSym = String
  repr TList = List MalVal
  repr TFn = List MalVal -> MalVal
  repr TVec = List MalVal
  repr TMap = List (MalVal, MalVal)

  public export
  record MalVal where
    constructor Mv
    type : MalType
    val : repr type

public export
Show (MalVal -> MalVal) where
  show x = "Function"

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

export
mfun : (List MalVal -> MalVal) -> MalVal
mfun = Mv TFn

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
