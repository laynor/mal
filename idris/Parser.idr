module Mal.Parser


import Text.Parser

export
terminal' : (ty -> Bool) -> Grammar ty True ty
terminal' f = terminal (\x => if (f x)
                              then Just x
                              else Nothing)

export
exactly : Eq ty => ty -> ty2 -> Grammar ty True ty2
exactly expected output = map (const output) (terminal' (==expected))
-- exactly input output = do res <- terminal' (==input)
--                           pure output

export
space : Grammar Char True ()
space = do terminal' isSpace
           pure ()

||| Consume and return the token
export
any : Grammar ty True ty
any = terminal (\x => Just x)

||| Parse a digit
export
digit : Grammar Char True Char
digit = terminal' isDigit

export
skip : Grammar ity c oty -> Grammar ity c ()
skip g = map (const ()) g
