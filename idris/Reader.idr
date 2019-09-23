module Reader

import Text.Parser
import Data.String
import Parser

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
             | TStr String
             | TNum Integer
             | TSym String

  Eq Token where
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

  Show Token where
    show token = ?rhs

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

  ||| Parse one or more whitespace character
  spaces : Grammar Char True ()
  spaces = do some space
              pure ()

  ||| Parse zero or more whitespace characters
  maybeSpaces : Grammar Char False ()
  maybeSpaces = do many space
                   pure ()


  ||| Parse a comment
  comment : Grammar Char True ()
  comment = do skip ';'
               skip $ many (skip notNewLine)
    where
      notNewLine : Char -> Bool
      notNewLine c = not $ elem c (unpack "\r\n")


  ignore : Grammar Char True ()
  ignore = spaces <|> comment


  numOrSym : Grammar Char True Token
  numOrSym = do res <- some symbolChar
                pure $ toToken (pack res)
    where
      toToken : String -> Token
      toToken acc = case parseInteger acc of
                          Just n => TNum n
                          Nothing => TSym acc

      symbolChar : Grammar Char True Char
      symbolChar = terminal' (\c => not (isSpecial c || isSpace c || c == '"'))


  escapeChar : Char -> Char
  escapeChar 'r' = '\r'
  escapeChar 'n' = '\n'
  escapeChar 't' = '\t'
  escapeChar 'b' = '\b'
  escapeChar c   = c


  string : Grammar Char True Token
  string = do exactly '"' ()
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
            quasiquote

  ||| All tokens
  partial
  token : Grammar Char True Token
  token = special <|> numOrSym <|> string

  %default partial
  -- TODO clean this one up
  ||| Token sequence.
  tokens : Grammar Char True (List Token)
  tokens = t00 <|> t01 <|> t1 <|> t2 <|> t3 <|> t4 <|> t5 <|> t6
    where
      t1 : Grammar Char True (List Token)
      t1 = do maybeSpaces
              sy <- numOrSym
              maybeSpaces
              sp <- special
              res <- tokens
              pure $ sy :: sp :: res
      t1_2 : Grammar Char True (List Token)
      t1_2 = do maybeSpaces
                sy <- numOrSym
                maybeSpaces
                sp <- string
                res <- tokens
                pure $ sy :: sp :: res

      t2 : Grammar Char True (List Token)
      t2 = do maybeSpaces
              sy <- numOrSym
              space
              res <- tokens
              pure $ sy :: res


      t3 : Grammar Char True (List Token)
      t3 = do maybeSpaces
              sp <- special
              res <- tokens
              pure (sp :: res)


      t4 : Grammar Char True (List Token)
      t4 = do maybeSpaces
              sy <- numOrSym
              maybeSpaces
              eof
              pure [sy]

      t5 : Grammar Char True (List Token)
      t5 = do maybeSpaces
              sp <- special
              maybeSpaces
              eof
              pure [sp]

      t6 : Grammar Char True (List Token)
      t6 = do maybeSpaces
              sy <- numOrSym
              maybeSpaces
              sp <- special
              eof
              pure [sy, sp]

      t00 : Grammar Char True (List Token)
      t00 = do maybeSpaces
               s <- string
               maybeSpaces
               eof
               pure [s]

      t01 : Grammar Char True (List Token)
      t01 = do maybeSpaces
               s <- string
               res <- tokens
               pure $ s::res


testInput : String
testInput = "(foo bar baz :foo 123 ~@!!('`(antani) 123antani antani123 1+))\"foobar\"baz"

test : String
test = case parse tokens (unpack testInput) of
            (Left l) => "Could not parse"
            (Right (res, [])) => if length res == length expectedResult && all (\(x, y) => x == y) (zip res expectedResult)
                                 then "OK"
                                 else "Wrong"
  where
    expectedResult : List Token
    expectedResult = [TOpenParen, TSym "foo", TSym "bar", TSym "baz", TSym ":foo", TNum 123, TUnquote, TDeref,
                      TSym "!!", TOpenParen, TQuote, TQuasiQuote, TOpenParen, TSym "antani", TCloseParen,
                      TSym "123antani", TSym "antani123", TSym "1+", TCloseParen, TCloseParen, TStr "foobar", TSym "baz"]
