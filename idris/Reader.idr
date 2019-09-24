module Reader

import Text.Parser
import Data.String
import Parser
import Core

%default total

namespace Lexer
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
             | TCaret
             | TStr String
             | TNum Integer
             | TSym String
  Show Token where
    show TCaret = "TCaret"
    show TOpenParen = "TOpenParen"
    show TCloseParen = "TCloseParen"
    show TOpenBracket = "TOpenBracket"
    show TCloseBracket = "TCloseBracket"
    show TOpenBrace = "TOpenBrace"
    show TCloseBrace = "TCloseBrace"
    show TQuote = "TQuote"
    show TQuasiQuote = "TQuasiQuote"
    show TUnquote = "TUnquote"
    show TDeref = "TDeref"
    show (TStr x) = ("TStr " ++ show x)
    show (TNum x) = ("TNum" ++ show x)
    show (TSym x) = ("TSym" ++ x)

  Eq Token where
    (==) TCaret TCaret = True
    (==) TOpenParen TOpenParen = True
    (==) TCloseParen TCloseParen = True
    (==) TOpenBracket TOpenBracket = True
    (==) TCloseBracket TCloseBracket = True
    (==) TOpenBrace TOpenBrace = True
    (==) TCloseBrace TCloseBrace = True
    (==) TQuote TQuote = True
    (==) TQuasiQuote TQuasiQuote = True
    (==) TUnquote TUnquote = True
    (==) TDeref TDeref = True
    (==) (TStr x) (TStr y) = x == y
    (==) (TNum x) (TNum y) = x == y
    (==) (TSym x) (TSym y) = x == y
    (==) _ _ = False

  ||| Matches MAL special Characters (which are tokens on their own)
  isSpecial : Char -> Bool
  isSpecial x = elem x (unpack "(){}[]~'`@")

  -- TODO: optional sign

  ||| Parse an open paren token
  openParen : Grammar Char True Token
  openParen = exactly '(' TOpenParen

  ||| Parse an open bracket token
  openBracket : Grammar Char True Token
  openBracket = exactly '[' TOpenBracket

  ||| Parse an open brace token
  openBrace : Grammar Char True Token
  openBrace = exactly '{' TOpenBrace

  ||| Parse a close paren token
  closeParen : Grammar Char True Token
  closeParen = exactly ')' TCloseParen

  ||| Parse an close bracket token
  closeBracket : Grammar Char True Token
  closeBracket = exactly ']' TCloseBracket

  ||| Parse an close brace token
  closeBrace : Grammar Char True Token
  closeBrace = exactly '}' TCloseBrace

  ||| Parse a deref (@) token
  deref : Grammar Char True Token
  deref = exactly '@' TDeref

  ||| Parse a quote (') token
  quote : Grammar Char True Token
  quote = exactly '\'' TQuote

  ||| Parse an unquote (~) token
  unquote : Grammar Char True Token
  unquote = exactly '~' TUnquote

  ||| Parse a quasiquote (`) token
  quasiquote : Grammar Char True Token
  quasiquote = exactly '`' TQuasiQuote

  comma : Grammar Char True ()
  comma = skip ','

  caret : Grammar Char True Token
  caret = exactly '^' TCaret

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
                           then TNum n
                           else TSym accStr
                 Nothing => TSym accStr

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
                                      pure $ TStr (pack acc)
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
  atom : Grammar Token True MalType
  atom = astring <|> ans
    where
      astring : Grammar Token True MalType
      astring = terminal (\x => case x of
                                     TStr x => Just (MStr x)
                                     _ => Nothing)

      ans : Grammar Token True MalType
      ans = terminal (\x => case x of
                                 TSym x => pure $ MSym x
                                 TNum x => pure $ MInt x
                                 _ => Nothing)

  openParen : Grammar Token True ()
  openParen = skip TOpenParen

  closeParen : Grammar Token True ()
  closeParen = exactly TCloseParen ()

  mutual
    -- XXX: The following definition is not total according to the idris totality checker
    -- list = do openParen
    --           res <- many form
    --           closeParen
    --           pure $ MList res

    -- Separating the rest of the list in a separate parser seems to do the trick
    delimitedRest : Token -> (List MalType -> MalType) -> Grammar Token True MalType
    delimitedRest closeDelim ctor  = do contents <- many form
                                        skip closeDelim
                                        pure $ ctor contents

    delimited : (opDelim : Token) -> (clDelim : Token) -> (List MalType -> MalType) -> Grammar Token True MalType
    delimited opDelim clDelim ctor = do skip opDelim
                                        delimitedRest clDelim ctor

    list : Grammar Token True MalType
    list = delimited TOpenParen TCloseParen MList

    vector : Grammar Token True MalType
    vector = delimited TOpenBracket TCloseBracket MVec

    mapLit : Grammar Token True MalType
    mapLit = do skip TOpenBrace
                mapLitRest
      where
        keyValue : Grammar Token True (MalType, MalType)
        keyValue = do k <- form
                      v <- form
                      pure (k, v)

        mapLitRest : Grammar Token True MalType
        mapLitRest = do kvs <- many keyValue
                        skip TCloseBrace
                        pure $ MMap kvs

    applyIf : Token -> String -> Grammar Token True MalType
    applyIf t symname = do skip t
                           f <- form
                           pure $ MList [MSym symname, f]


    quotedForm : Grammar Token True MalType
    quotedForm = applyIf TQuote "quote"

    quasiQuotedForm : Grammar Token True MalType
    quasiQuotedForm = applyIf TQuasiQuote "quasiquote"

    unquoteForm : Grammar Token True MalType
    unquoteForm = applyIf TUnquote "unquote"

    derefForm : Grammar Token True MalType
    derefForm = applyIf TDeref "deref"

    spliceUnquote : Grammar Token True MalType
    spliceUnquote = do skip TUnquote
                       applyIf TDeref "splice-unquote"

    withMeta : Grammar Token True MalType
    withMeta = do skip TCaret
                  m <- form
                  f <- form
                  pure $ MList [MSym "with-meta", f, m]


    form : Grammar Token True MalType
    form = atom <|> list <|> vector <|> mapLit <|> withMeta <|> quotedForm <|> spliceUnquote <|> quasiQuotedForm <|> quasiQuotedForm <|> unquoteForm <|> derefForm

export
readString : String -> MalType
readString input = case parse tokens (unpack input) of
                        (Left (Error msg [])) => MError "Error: unexpected end of input."
                        (Left (Error msg (x :: xs))) => MError $ "Error: unexpected end of input " ++ (show msg)
                        (Right (ts, morechars)) => case parse form ts of
                                                        (Left (Error msg' [])) => MError $ "Error: unexpected end of input."
                                                        (Left (Error msg' (x :: xs))) => MError $ "Error: unexpected end of input. " ++ msg' ++ "\nRemaining input: " ++ (show (x::xs))
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
    expectedResult = [TOpenParen, TSym "foo", TSym "bar", TSym "baz", TSym ":foo", TNum 123, TUnquote, TDeref,
                      TSym "!!", TOpenParen, TQuote, TQuasiQuote, TOpenParen, TSym "antani", TCloseParen,
                      TSym "123antani", TSym "antani123", TSym "1+", TCloseParen, TCloseParen, TStr "foobar", TSym "baz"]

partial
tokenize : String -> List Token
tokenize x = case parse tokens (unpack x) of
                  (Right (a, b)) => a
