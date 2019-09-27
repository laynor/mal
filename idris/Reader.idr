module Reader

import Text.Parser
import Data.String
import Parser
import Types
import Core

%default total

namespace Lexer
  data Token = TkOpenParen
             | TkCloseParen
             | TkOpenBracket
             | TkCloseBracket
             | TkOpenBrace
             | TkCloseBrace
             | TkQuote
             | TkQuasiQuote
             | TkUnquote
             | TkDeref
             | TkCaret
             | TkStr String
             | TkNum Integer
             | TkSym String
  Show Token where
    show TkCaret = "TkCaret"
    show TkOpenParen = "TkOpenParen"
    show TkCloseParen = "TkCloseParen"
    show TkOpenBracket = "TkOpenBracket"
    show TkCloseBracket = "TkCloseBracket"
    show TkOpenBrace = "TkOpenBrace"
    show TkCloseBrace = "TkCloseBrace"
    show TkQuote = "TkQuote"
    show TkQuasiQuote = "TkQuasiQuote"
    show TkUnquote = "TkUnquote"
    show TkDeref = "TkDeref"
    show (TkStr x) = ("TkStr " ++ show x)
    show (TkNum x) = ("TkNum" ++ show x)
    show (TkSym x) = ("TkSym" ++ x)

  Eq Token where
    (==)  TkCaret         TkCaret         =       True
    (==)  TkOpenParen     TkOpenParen     =       True
    (==)  TkCloseParen    TkCloseParen    =       True
    (==)  TkOpenBracket   TkOpenBracket   =       True
    (==)  TkCloseBracket  TkCloseBracket  =       True
    (==)  TkOpenBrace     TkOpenBrace     =       True
    (==)  TkCloseBrace    TkCloseBrace    =       True
    (==)  TkQuote         TkQuote         =       True
    (==)  TkQuasiQuote    TkQuasiQuote    =       True
    (==)  TkUnquote       TkUnquote       =       True
    (==)  TkDeref         TkDeref         =       True
    (==)  (TkStr x)       (TkStr  y)      =  x  ==  y
    (==)  (TkNum x)       (TkNum  y)      =  x  ==  y
    (==)  (TkSym x)       (TkSym  y)      =  x  ==  y
    (==)  _               _               =       False

  ||| Matches MAL special Characters (which are tokens on their own)
  isSpecial : Char -> Bool
  isSpecial x = elem x (unpack "(){}[]~'`@")

  -- TODO: optional sign

  ||| Parse an open paren token
  openParen : Grammar Char True Token
  openParen = exactly '(' TkOpenParen

  ||| Parse an open bracket token
  openBracket : Grammar Char True Token
  openBracket = exactly '[' TkOpenBracket

  ||| Parse an open brace token
  openBrace : Grammar Char True Token
  openBrace = exactly '{' TkOpenBrace

  ||| Parse a close paren token
  closeParen : Grammar Char True Token
  closeParen = exactly ')' TkCloseParen

  ||| Parse an close bracket token
  closeBracket : Grammar Char True Token
  closeBracket = exactly ']' TkCloseBracket

  ||| Parse an close brace token
  closeBrace : Grammar Char True Token
  closeBrace = exactly '}' TkCloseBrace

  ||| Parse a deref (@) token
  deref : Grammar Char True Token
  deref = exactly '@' TkDeref

  ||| Parse a quote (') token
  quote : Grammar Char True Token
  quote = exactly '\'' TkQuote

  ||| Parse an unquote (~) token
  unquote : Grammar Char True Token
  unquote = exactly '~' TkUnquote

  ||| Parse a quasiquote (`) token
  quasiquote : Grammar Char True Token
  quasiquote = exactly '`' TkQuasiQuote

  comma : Grammar Char True ()
  comma = skip ','

  caret : Grammar Char True Token
  caret = exactly '^' TkCaret

  ||| Parse a comment
  comment : Grammar Char True ()
  comment = do skip ';'
               skip $ many (skip notNewLine)
    where
      notNewLine : Char -> Bool
      notNewLine c = not $ elem c (unpack "\r\n")

  whitespace : Grammar Char True ()
  whitespace = space <|> comma <|> comment

  ||| Parse one or more whitespace character
  spaces : Grammar Char True ()
  spaces = skip $ some whitespace

  ||| Parse zero or more whitespace characters
  maybeSpaces : Grammar Char False ()
  maybeSpaces = skip $ many whitespace

  isMalSpace : Char -> Bool
  isMalSpace c = isSpace c || c == ','

  symbolChar : Grammar Char True Char
  symbolChar = terminal' (\c => not (isSpecial c || isMalSpace c || c == '"' || c == ',' || c == ';' || c == '^'))

  numOrSym : Grammar Char True Token
  numOrSym = do res <- some symbolChar
                pure $ toToken res -- XXX: rewriting this as map (toToken . pack) (some symbolChar) does not work
    where
      toToken : List Char -> Token
      toToken acc =
        let accStr = (pack acc) in
            case parseInteger accStr of
                 Just n => if any isDigit acc
                           then TkNum n
                           else TkSym accStr
                 Nothing => TkSym accStr

  escapeChar : Char -> Char
  escapeChar 'r' = '\r'
  escapeChar 'n' = '\n'
  escapeChar 't' = '\t'
  escapeChar 'b' = '\b'
  escapeChar c   = c

  string : Grammar Char True Token
  string = do skip '"'
              stringRest []
    where
      stringRest : List Char -> Grammar Char True Token
      stringRest acc = do firstChar <- peek
                          case firstChar of
                            '"' => do any
                                      pure $ TkStr (pack acc)
                            '\\' => do any
                                       c <- any
                                       stringRest (acc ++ [escapeChar c])
                            _ => do any
                                    stringRest (acc ++ [firstChar])


  ||| Characters that are tokens on their own
  special : Grammar Char True Token
  special = openParen <|>
            closeParen <|>
            openBracket <|>
            closeBracket <|>
            openBrace <|>
            closeBrace <|>
            deref <|>
            quote <|>
            unquote <|>
            quasiquote <|>
            caret

  ||| All tokens
  token : Grammar Char True Token
  token = special <|> numOrSym <|> string

  -- TODO clean this one up
  ||| Token sequence.
  tokens : Grammar Char True (List Token)
  tokens = stringEof <|> string' <|> nsSpecial  <|> nsStringEof <|> nsString <|> nsWs <|> special' <|> nsEof <|> specialEof <|> nsSpecialEof
    where
      skipws : Grammar Char c oty -> Grammar Char c oty
      skipws g = seq maybeSpaces (const g)
      eofPure : ty -> Grammar Char False ty
      eofPure a = map (const a) (skipws eof)

      nsSpecial : Grammar Char True (List Token)
      nsSpecial = do sy <- skipws numOrSym
                     sp <- skipws special
                     res <- tokens
                     pure $ sy :: sp :: res

      nsString : Grammar Char True (List Token)
      nsString = do sy <- skipws numOrSym
                    sp <- skipws string
                    res <- tokens
                    pure $ sy :: sp :: res

      nsWs : Grammar Char True (List Token)
      nsWs = do sy <- skipws numOrSym
                spaces
                res <- tokens
                pure $ sy :: res

      special' : Grammar Char True (List Token)
      special' = do sp <- skipws special
                    res <- tokens
                    pure (sp :: res)

      nsEof : Grammar Char True (List Token)
      nsEof = do sy <- skipws numOrSym
                 eofPure [sy]

      specialEof : Grammar Char True (List Token)
      specialEof = do sp <- skipws special
                      eofPure [sp]

      nsSpecialEof : Grammar Char True (List Token)
      nsSpecialEof = do sy <- skipws numOrSym
                        sp <- skipws special
                        eofPure [sy, sp]

      nsStringEof : Grammar Char True (List Token)
      nsStringEof = do sy <- skipws numOrSym
                       st <- (skipws string)
                       eofPure [sy, st]

      stringEof : Grammar Char True (List Token)
      stringEof = do s <- skipws string
                     eofPure [s]

      string' : Grammar Char True (List Token)
      string' = do s <- skipws string
                   res <- tokens
                   pure $ s::res

namespace Parser
  atom : Grammar Token True MalVal
  atom = astring <|> ans
    where
      astring : Grammar Token True MalVal
      astring = terminal (\x => case x of
                                     TkStr x => Just (Mv TStr x)
                                     _ => Nothing)

      ans : Grammar Token True MalVal
      ans = terminal (\x => case x of
                                 TkSym x => Just $ Mv TSym x
                                 TkNum x => Just $ Mv TInt x
                                 _ => Nothing)

  openParen : Grammar Token True ()
  openParen = skip TkOpenParen

  closeParen : Grammar Token True ()
  closeParen = exactly TkCloseParen ()

  mutual
    -- XXX: The following definition is not total according to the idris totality checker
    -- list = do openParen
    --           res <- many form
    --           closeParen
    --           pure $ MList res

    -- Separating the rest of the list in a separate parser seems to do the trick
    delimitedRest : Token -> (List MalVal -> MalVal) -> Grammar Token True MalVal
    delimitedRest closeDelim ctor  = do contents <- many form
                                        skip closeDelim
                                        pure $ ctor contents

    delimited : (opDelim : Token) -> (clDelim : Token) -> (List MalVal -> MalVal) -> Grammar Token True MalVal
    delimited opDelim clDelim ctor = do skip opDelim
                                        delimitedRest clDelim ctor

    list : Grammar Token True MalVal
    list = delimited TkOpenParen TkCloseParen (Mv TList)

    vector : Grammar Token True MalVal
    vector = delimited TkOpenBracket TkCloseBracket (Mv TVec)

    mapLit : Grammar Token True MalVal
    mapLit = do skip TkOpenBrace
                mapLitRest
      where
        keyValue : Grammar Token True (MalVal, MalVal)
        keyValue = do k <- form
                      v <- form
                      pure (k, v)

        mapLitRest : Grammar Token True MalVal
        mapLitRest = do kvs <- many keyValue
                        skip TkCloseBrace
                        pure $ Mv TMap kvs

    applyIf : Token -> String -> Grammar Token True MalVal
    applyIf t symname = do skip t
                           f <- form
                           pure $ Mv TList [Mv TSym symname, f]


    quotedForm : Grammar Token True MalVal
    quotedForm = applyIf TkQuote "quote"

    quasiQuotedForm : Grammar Token True MalVal
    quasiQuotedForm = applyIf TkQuasiQuote "quasiquote"

    unquoteForm : Grammar Token True MalVal
    unquoteForm = applyIf TkUnquote "unquote"

    derefForm : Grammar Token True MalVal
    derefForm = applyIf TkDeref "deref"

    spliceUnquote : Grammar Token True MalVal
    spliceUnquote = do skip TkUnquote
                       applyIf TkDeref "splice-unquote"

    withMeta : Grammar Token True MalVal
    withMeta = do skip TkCaret
                  m <- form
                  f <- form
                  pure $ Mv TList [Mv TSym "with-meta", f, m]


    form : Grammar Token True MalVal
    form = atom <|> list <|> vector <|> mapLit <|> withMeta <|> quotedForm <|> spliceUnquote <|> quasiQuotedForm <|> quasiQuotedForm <|> unquoteForm <|> derefForm

export
readString : String -> MalVal
readString input = case parse tokens (unpack input) of
                        (Left (Error msg [])) => Mv TErr "Error: unexpected end of input."
                        (Left (Error msg (x :: xs))) => Mv TErr $ "Error: unexpected end of input " ++ (show msg)
                        (Right (ts, morechars)) => case parse form ts of
                                                        (Left (Error msg' [])) => Mv TErr $ "Error: unexpected end of input."
                                                        (Left (Error msg' (x :: xs))) => Mv TErr $ "Error: unexpected end of input. " ++ msg' ++ "\nRemaining input: " ++ (show (x::xs))
                                                        (Right (malData, b)) => malData

testInput : String
testInput = "(foo bar baz :foo 123 ~@!!('`(antani) 123antani antani123 1+))\"foobar\"baz"

test : String
test = case parse tokens (unpack testInput) of
            (Left l) => "Could not parse"
            (Right (res, [])) => if length res == length expectedResult && all (\(x, y) => x == y) (zip res expectedResult)
                                 then "OK"
                                 else "Wrong"
            (Right (res, _)) => "Incomplete"
  where
    expectedResult : List Token
    expectedResult = [TkOpenParen, TkSym "foo", TkSym "bar", TkSym "baz", TkSym ":foo", TkNum 123, TkUnquote, TkDeref,
                      TkSym "!!", TkOpenParen, TkQuote, TkQuasiQuote, TkOpenParen, TkSym "antani", TkCloseParen,
                      TkSym "123antani", TkSym "antani123", TkSym "1+", TkCloseParen, TkCloseParen, TkStr "foobar", TkSym "baz"]

partial
tokenize : String -> List Token
tokenize x = case parse tokens (unpack x) of
                  (Right (a, b)) => a
