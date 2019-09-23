module Mal.Reader

import Text.Parser
import Data.String

%default total

isStringDelim : Char -> Bool
isStringDelim = (=='"')

isSpecial : Char -> Bool
isSpecial x = elem x (unpack "(){}[]~'`@")

terminal' : (ty -> Bool) -> Grammar ty True ty
terminal' f = terminal (\x => if (f x)
                              then Just x
                              else Nothing)

digit : Grammar Char True Char
digit = terminal' isDigit

number : Grammar Char True Integer
number = do digits <- some digit
            pure (cast (pack digits))

notSpaceOrSpecial : Grammar Char True Char
notSpaceOrSpecial = terminal' (\c => not (isSpecial c || isSpace c))

identifier : Grammar Char True (List Char)
identifier = some notSpaceOrSpecial

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

space : Grammar Char True ()
space = do terminal' isSpace
           pure ()

spaces : Grammar Char True ()
spaces = do some space
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
