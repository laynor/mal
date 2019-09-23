module Mal.Reader

import Text.Parser
import Data.String

%default total

isStringDelim : Char -> Bool
isStringDelim = (=='"')
isSpecial : Char -> Bool
isSpecial x = elem x (unpack "(){}[]~'`@")

-- tokenizeString : List Char -> Maybe (List Char, List Char)
-- -- tokenizeString [] = Nothing

-- partial
-- tokenizeLst : List Char -> List Char -> List (List Char)
-- tokenizeLst [] []  = []
-- tokenizeLst [] acc = [acc]
-- tokenizeLst (c::input) [] = if isStringDelim c
--                             then let res = tokenizeString input in
--                                      case res of
--                                        | Just s => s :: tokenizeLst rest []
--                                        | Nothing  => Nothing
--                             else if isSpecial c
--                             then [c] :: tokenizeLst input []
--                             else if isSpace c
--                             then tokenizeLst input []
--                             else tokenizeLst input [c]

-- tokenizeLst (c::input) acc = if isSpecial c || isStringDelim c
--                              then acc :: tokenizeLst (c :: input) []
--                              else if isSpace c
--                              then acc :: tokenizeLst (c :: input) []
--                              else tokenizeLst input (acc ++ [c])


-- partial
-- tokenize : String -> List String
-- tokenize input = map pack (tokenizeLst (unpack input) [])

terminal' : (ty -> Bool) -> Grammar ty True ty
terminal' f = terminal (\x => if (f x)
                              then Just x
                              else Nothing)

many1 : Grammar ty True ty2 -> Grammar ty True (List ty2)
many1 g = do r <- g
             rs <- many g
             pure $ r :: rs

digit : Grammar Char True Char
digit = terminal' isDigit

number : Grammar Char True Integer
number = do digits <- many1 digit
            pure (cast (pack digits))

notSpaceOrSpecial : Grammar Char True Char
notSpaceOrSpecial = terminal' (\c => not (isSpecial c || isSpace c))

identifier : Grammar Char True (List Char)
identifier = many1 notSpaceOrSpecial

data Token = TOpenParen
           | TCloseParen
           | TOpenBracket
           | TCloseBracket
           | TOpenBrace
           | TCloseBrace
           | TQuote
           | TQuasiQuote
           | TUnquote
           | TDeref
           | TStr String
           | TNum Integer
           | TSym String

exactly : Eq ty => ty -> ty2 -> Grammar ty True ty2
exactly input output = do res <- terminal' (==input)
                          pure output

openParen : Grammar Char True Token
openParen = exactly '(' TOpenParen

openBracket : Grammar Char True Token
openBracket = exactly '[' TOpenBracket

openBrace : Grammar Char True Token
openBrace = exactly '{' TOpenBrace

closeParen : Grammar Char True Token
closeParen = exactly ')' TCloseParen

closeBracket : Grammar Char True Token
closeBracket = exactly ']' TCloseBracket

closeBrace : Grammar Char True Token
closeBrace = exactly '}' TCloseBrace

deref : Grammar Char True Token
deref = exactly '@' TDeref

quote : Grammar Char True Token
quote = exactly '\'' TQuote

unquote : Grammar Char True Token
unquote = exactly '~' TUnquote

quasiquote : Grammar Char True Token
quasiquote = exactly '`' TQuasiQuote

tnumber : Grammar Char True Token
tnumber = do res <- number
             pure $ TNum res

-- tsymbol : Grammar Char True Token
-- tsymbol = do res <- identifier
--              pure $ TSym (pack res)

space : Grammar Char True ()
space = do terminal' isSpace
           pure ()

spaces : Grammar Char True ()
spaces = do many1 space
            pure ()

maybeSpaces : Grammar Char False ()
maybeSpaces = do many space
                 pure ()

isNewLine : Char -> Bool
isNewLine c = elem c (unpack "\r\n")

comment : Grammar Char True ()
comment = do exactly ';' ()
             many (terminal' (not . isNewLine))
             pure ()

ignore : Grammar Char True ()
ignore = spaces <|> comment


toToken : String -> Token
toToken acc = case parseInteger acc of
                   Just n => TNum n
                   Nothing => TSym acc

partial
numOrSym : Grammar Char True (List Char)
numOrSym = some (terminal' (\x => not (isSpecial x || isSpace x)))

ns : Grammar Char True Token
ns = do res <- numOrSym
        pure $ toToken (pack res)

{-
partial
numberOrSymbol : List Char -> Grammar Char False Token
numberOrSymbol acc = do c <- peek
                        if isSpecial c || isSpace c
                        then pure $ toToken (pack acc)
                        else do terminal' (\x => True)
                                numberOrSymbol (acc ++ [c])

partial
ns : Grammar Char True Token
ns = do c <- terminal' (not . isSpecial)
        numberOrSymbol [c]
-}

any : Grammar ty True ty
any = terminal (\x => Just x)


escapeChar : Char -> Char
escapeChar 'r' = '\r'
escapeChar 'n' = '\n'
escapeChar 't' = '\t'
escapeChar 'b' = '\b'
escapeChar c   = c

stringRest : List Char -> Grammar Char True (List Char)
stringRest acc = do firstChar <- peek
                    case firstChar of
                      '"' => do any
                                pure acc
                      '\\' => do any
                                 c <- any
                                 stringRest (acc ++ [escapeChar c])
                      _ => do any
                              stringRest (acc ++ [firstChar])

tstring : Grammar Char True Token
tstring = do exactly '"' ()
             res <- stringRest'
             pure res
  where
    stringRest' : Grammar Char True Token
    stringRest' = do lst <- stringRest []
                     pure $ TStr (pack lst)

special : Grammar Char True Token
special = openParen <|> closeParen <|> openBracket <|> closeBracket <|> openBrace <|> closeBrace <|> deref <|> quote <|> unquote <|> quasiquote

partial
token : Grammar Char True Token
token = special <|> ns

%default partial
tokens : Grammar Char True (List Token)
tokens = t1 <|> t2 <|> t3 <|> t4 <|> t5 <|> t6
  where
    t3 : Grammar Char True (List Token)
    t3 = do maybeSpaces
            sp <- special
            res <- tokens
            pure (sp :: res)

    t6 : Grammar Char True (List Token)
    t6 = do maybeSpaces
            sy <- ns
            maybeSpaces
            sp <- special
            eof
            pure [sy, sp]


    t1 : Grammar Char True (List Token)
    t1 = do maybeSpaces
            sy <- ns
            maybeSpaces
            sp <- special
            res <- tokens
            pure $ sy :: sp :: res

    t2 : Grammar Char True (List Token)
    t2 = do maybeSpaces
            sy <- ns
            space
            res <- tokens
            pure $ sy :: res

    t4 : Grammar Char True (List Token)
    t4 = do maybeSpaces
            sy <- ns
            maybeSpaces
            eof
            pure [sy]

    t5 : Grammar Char True (List Token)
    t5 = do maybeSpaces
            sp <- special
            maybeSpaces
            eof
            pure [sp]

-- TOK -> maybeSpaces SYM maybeSpaces SPECIAL TOK
-- TOK -> maybeSpaces SYM maybeSpaces SPACE TOK
-- TOK -> maybeSpaces SPECIAL TOK
-- TOK -> maybeSpaces SYM maybeSpaces eof
-- TOK -> maybeSpaces SPECIAL maybeSpaces eof
