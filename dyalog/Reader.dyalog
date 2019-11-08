 :Require file://debug.dyalog
:Require file://Chars.dyalog
:Require file://Types.dyalog

⍝ Try another lexing technique:
⍝ get bool vectors like
⍝ digits←⍵∊'1234567890'
⍝ spaces←2×⍵∊C.WSNL
⍝ special←4×⍵∊C.Special
⍝ etc.

⍝ Also: try representing parsers as tables.

 :Namespace Reader
   T←##.Types
   C←##.Chars
   Env←##.Env
   ⍝ Debug
   ⍝ Parser combinators
   ⍝ ==================
   ⍝ Parsers are functions that take an ⍝ array of tokens and return an array of
   ⍝ 3 values:
   ⍝
   ⍝ s(tatus) |r(esult)| R(est)     ←  parser input
   ⍝----------|--------|-----------|
   ⍝    1     | result | (k↓input) | -→ SUCCESS
   ⍝    0     | empty  | input     | -→ FAILURE

   Ok←1                          ⍝ Results are array of 3 values:
   fail←{0 (0⍴⍵) ⍵}              ⍝ (0 or 1: failure or success)

   eof←(0=≢),(⊂0∘⍴),⊂            ⍝ parses the end of file
   ∆t←{                          ⍝ {⍵∊⎕D} ∆t <--> parses a character if it is a digit
     0=≢⍵: fail ⍵
     ⍺⍺ ⊃⍵: Ok (⊃⍵) (1↓⍵)
     fail ⍵
   }
   only←{(=∘⍺⍺) ∆t ⍵}            ⍝ 'W' only <--> parses only the character 'W'
   map←{s r R←⍵⍵ ⍵ ⋄ s: s (⍺⍺ r) R ⋄ fail ⍵}
   pipe←{⍵⍵ map ⍺⍺ ⍵}


   ⍝ sequences
   seq←{                         ⍝ sequence opening
     s1 r1 R1←⍺⍺ ⍵

     s1: r1 map ⍵⍵ R1
     fail ⍵
   }

   sq←{                          ⍝ other elements in the sequence
     s r R←⍺⍺ ⍵

     s: (r,⊂) map ⍵⍵ R
     fail ⍵
   }

   str←{
     S←⍺⍺
     0=≢⍺⍺: Ok '' ⍵
     {
       x←⊃S
       s r R←x only ⍵
       s: {r,⍵} map ((1↓S)str) R
       fail ⍵
     }⍵
   }

   many←{                        ⍝ parse 0 or more occurrences of ⍺⍺
     P←⍺⍺ ⋄ s r R1←⍺⍺ ⍵

     s: {
       s rs R2←P many R1
       Ok  ((⊂r),rs) R2
     }⍬
     Ok r R1
   }

   some←{((⊃,/) map ((⊂ map ⍺⍺) seq (⍺⍺ many))) ⍵} ⍝ 1 or more ⍺⍺
   ⍝ returns the first parser that succeeds
   alt←{s r R←⍺⍺ ⍵ ⋄ s: s r R ⋄ ⍵⍵ ⍵}

   or←{(1+>/(≢3∘⊃)¨R)⊃R←(⍺⍺ ⍵) (⍵⍵ ⍵)} ⍝ returns result of the parser that
   ⍝ consumed more tokens (greedy)
   skip←{(⊢∘(0⍴⍵)) map ⍺⍺ ⍵} ⍝ can't use yet, as sequencing operations die as ⍝ a
   ⍝ result of using this one.

   flat←{∊map ⍺⍺ ⍵} ⍝ Not always the right way to do it - gotta find the right
   ⍝ definition

   ⍝ READER
   ⍝ ======

   ⍝ Example lexers
   isAlpha←{⍵∊'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'}
   isDigit←{⍵∊'0123456789'}
   isAlphaNum←isAlpha∨isDigit
   DIGIT←isDigit ∆t
   alpha←isAlpha ∆t
   alphaNum←isAlphaNum ∆t


   ⍝ Lexing - basic chars


   NL←∊∘C.NL ∆t
   isWS1←∊∘C.WSNL
   isWS←∧/∘isWS1
   WS1←isWS1 ∆t
   WS←WS1 some flat

   ⍝ MAL TOKENS
   ⍝ ----------

   ⍝ Strings
   escape←{                   ⍝ we append ⍵ (the second char in the
     escapes←'trbn',⍵         ⍝ escape sequence) so that it is
     trans←C.(TAB CR BS LF),⍵ ⍝ translated to itself when it's not a
     1↑(⍵=escapes)/trans          ⍝ known escape sequence
   }

   ⍝ STRING : parse a string literal, escaped with the usual C rules.
   ⍝          returns the escaped string
   STRING←{
     REST←{
       c←1↑⍵
       y←1↑1↓⍵

       0=≢⍵:  fail inp
       c='"': Ok '' (1↓⍵)
       c='\': (escape y)∘, map ∇ 2↓⍵
       {c,⍵} map ∇ 1↓⍵
     }

     {⊃1↓⍵} map ('"' only seq REST) ⍵
   }


   ⍝ QSTRING : parse a string literal, return the string literal itself
   QSTRING←{
     REST←{
       c←1↑⍵
       cc←2↑⍵

       0=≢⍵: fail inp
       c='\': ,⍨∘2↑⍵ map ∇ 2↓⍵
       c='"': Ok '"' (1↓⍵)
       {c,⍵} map ∇ 1↓⍵
     }
     {∊⍵} map ('"' only seq REST) ⍵
   }

   SEMI←';'only
   specialChars←'''~@`()[]{}^'
   nonSymbol←specialChars,'";,',C.WSNL
   SYMCHAR_NOT_DIGIT←{~⍵∊nonSymbol,⎕D,'.'}∆t
   SYMCHAR←(~∊∘nonSymbol)∆t
   COMMA←(=∘',')∆t

   COMMENT←(SEMI seq ({~⍵∊C.NL}∆t many) sq NL) flat

   SIGN←∊∘'+-' ∆t
   INT←(DIGIT some) or (('-'⎕r'¯')∘∊ map (SIGN sq (DIGIT some)))
   SYM←∊map ((DIGIT many) seq SYMCHAR_NOT_DIGIT sq (SYMCHAR many))
   TRUE←{T.Bool 1} map ('true'str)
   FALSE←{T.Bool 0} map ('false'str)

   SPECIAL←∊∘specialChars ∆t

   flt←{(⍺⍺¨⍵)/⍵}


   isComma←≡∘','
   isComment←';'≡⊃
   isWSOrComment←isWS∨isComment∨isComma

   toInt←{0<≢⍵: ⍎⍵ ⋄ ⍵}

   ⍝ Tokens represented as a pair TokType value


   tokType←⊃
   tokVal←{1↑1↓⍵}

   tok←       WS or COMMENT or COMMA or TRUE or FALSE
   tok←tok or (T.Special map SPECIAL)
   tok←tok or ((T.Number,toInt) map INT)
   tok←tok or (T.Symbol map SYM)
   tok←tok or (T.String map STRING)

   tokens←(~isWSOrComment) flt map (tok many)

   tt←{x←⍺ ⋄ {x=tokType⍵}∆t ⍵}
   String←T.String∘tt
   Bool←T.Bool∘tt
   Num←T.Number∘tt
   Sym←T.Symbol∘tt
   isSpecial←{ty val←⍺ ⋄ (ty=T.Special)∧(val≡⍵)} ⍝ Ex: (Special '~') isSpecial '~' <--> 1
   Spec←{(isSpecial∘⍺⍺)∆t ⍵}

   ⍝ In most of these parsers, ⍺⍺ is mForm. I don't know if there's a better way to
   ⍝ do mutual recursion

   hDelim←{{(⊃1↓⍵)} map ((⊂map(⍺⍺[1] Spec)) sq (⍵⍵ many) sq (⍺⍺[2] Spec)) ⍵}

   List←{T.List map ('()' hDelim ⍺⍺) ⍵}
   Vec←{T.Vec map ('[]' hDelim ⍺⍺) ⍵}
   Map←{T.Map map ('{}' hDelim ⍺⍺) ⍵}

   mkFnAppl←{T.List ((⊂T.Symbol ⍺),⍵)}

   specialHelper←{
     s r R←(((⊃⍵⍵) Spec) seq ⍺⍺) ⍵
     s : Ok ((1↓⍵⍵) mkFnAppl 1↓r) R
     fail ⍵
   }

   Quote←{(⍺⍺ specialHelper '''quote') ⍵}
   Quasiquote←{(⍺⍺ specialHelper '`quasiquote')⍵}
   Deref←{(⍺⍺ specialHelper '@deref')⍵}
   UnquoteOrSpliceUnquote←{
     spu←{'splice-unquote' mkFnAppl 2↓⍵} map (('~' Spec) seq ('@' Spec) sq ⍺⍺)
     unq←{'unquote' mkFnAppl 1↓⍵} map(('~' Spec) seq ⍺⍺)
     spu or unq ⍵
   }

   WithMeta←{{T.List ((⊂T.Symbol 'with-meta'),⌽1↓⍵)} map ('^' Spec seq ⍺⍺ sq ⍺⍺) ⍵}

   Form←{
     p←Bool or Num or Sym or String
     p←p or (∇ List)  or (∇ Vec)        or (∇ Map)
     p←p or (∇ Quote) or (∇ Quasiquote) or (∇ UnquoteOrSpliceUnquote)
     p←p or (∇ Deref) or (∇WithMeta)

     p ⍵
   }

   read←{
     s r R←tokens ⍵
     s:   {
       s r R←Form ⍵
       s: r
       T.Error 'end of input'
     }r
     T.Error 'Lexer error'
   }
 :EndNamespace