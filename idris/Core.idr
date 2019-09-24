module Core

public export
data MalType = MInt Integer
             | MStr String
             | MSym String
             | MNil
             | MList (List MalType)
             | MVec (List MalType)
             | MMap (List (MalType, MalType))
             | MError String

public export
lcons : MalType -> (l : MalType) -> {auto prf : l = MList l'} -> MalType
lcons x (MList xs) {prf = Refl} = MList (x::xs)
