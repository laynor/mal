 :Require file://debug.dyalog
 :Namespace m
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
   foo←##.dbg.prn
   ∆t←{                          ⍝ {⍵∊⎕D} ∆t <--> parses a character if it is a digit
     0=≢⍵: fail ⍵
     ⍺⍺ ⊃⍵: Ok (⊃⍵) (1↓⍵)
     fail ⍵
   }
   only←{(=∘⍺⍺) ∆t ⍵}            ⍝ 'W' only <--> parses only the character 'W'
   map←{s r R←⍵⍵ ⍵ ⋄ s (⍺⍺ r) R}
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

   ⍝ Special chars
   :Namespace C
     BS←⎕ucs 8
     TAB←⎕ucs 9
     LF←⎕ucs 10
     CR←⎕ucs 13
     NL←⎕ucs 10
     SPC←' '
     WS←TAB,SPC
     WSNL←NL,WS
   :EndNamespace

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

   unescape←{
     unescape1←{
       escapes←(('\'∘,)¨'trbn\"'),⍵
       trans←C.(TAB CR BS LF),'\"',⍵
       1↑(⍵=trans)/escapes
     }
     ∊unescape1¨⍵
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

   INT←DIGIT some
   SYM←∊map ((DIGIT many) seq SYMCHAR_NOT_DIGIT sq (SYMCHAR many))

   SPECIAL←∊∘specialChars ∆t

   flt←{(⍺⍺¨⍵)/⍵}


   isComma←≡∘','
   isComment←';'≡⊃
   isWSOrComment←isWS∨isComment∨isComma

   toInt←{0<≢⍵: ⍎⍵ ⋄ ⍵}

   ⍝ Tokens represented as a pair TokType value
   Special Symbol Number String List Vec Map Error←⍳8

   tokType←⊃
   tokVal←{1↑1↓⍵}

   tok←       WS or COMMENT or COMMA
   tok←tok or ({Special ⍵} map SPECIAL)
   tok←tok or ({Number,toInt ⍵} map INT)
   tok←tok or ({Symbol ⍵} map SYM)
   tok←tok or ({String ⍵} map STRING)

   tokens←(~isWSOrComment) flt map (tok many)

   tt←{x←⍺ ⋄ {x=tokType⍵}∆t ⍵}
   mString←String∘tt
   mNum←Number∘tt
   mSym←Symbol∘tt
   isSpecial←{ty val←⍺ ⋄ (ty=Special)∧(val≡⍵)} ⍝ Ex: (Special '~') isSpecial '~' <--> 1
   mSpec←{(isSpecial∘⍺⍺)∆t ⍵}

   ⍝ In most of these parsers, ⍺⍺ is mForm. I don't know if there's a better way to
   ⍝ do mutual recursion

   ⍝ (spec applyToForm 'quote')
   ⍝ applyToForm←{({List (Symbol s) (⊃1↓⍵)} map ((⍵⍵ mSpec) seq ⍺⍺)) ⍵}

   mDelim←{{(⊃1↓⍵)} map ((⊂map(⍺⍺[1] mSpec)) sq (⍵⍵ many) sq (⍺⍺[2] mSpec)) ⍵}

   mList←{{List ⍵} map ('()' mDelim ⍺⍺) ⍵}
   mVec←{{Vec ⍵} map ('[]' mDelim ⍺⍺) ⍵}
   mMap←{{Map ⍵} map ('{}' mDelim ⍺⍺) ⍵}
   mkFnAppl←{List ((⊂Symbol ⍺),⍵)}
   specialHelper←{
     s r R←(((⊃⍵⍵) mSpec) seq ⍺⍺) ⍵
     s : Ok ((1↓⍵⍵) mkFnAppl 1↓r) R
     fail ⍵
   }


   mQuote←{(⍺⍺ specialHelper '''quote') ⍵}
   mQuasiquote←{(⍺⍺ specialHelper '`quasiquote')⍵}
   mDeref←{(⍺⍺ specialHelper '@deref')⍵}
   mUnquoteOrSpliceUnquote←{
     spu←{'splice-unquote' mkFnAppl 2↓⍵} map (('~' mSpec) seq ('@' mSpec) sq ⍺⍺)
     unq←{'unquote' mkFnAppl 1↓⍵} map(('~' mSpec) seq ⍺⍺)
     spu or unq ⍵
   }

   mWithMeta←{{List ((⊂Symbol 'with-meta'),⌽1↓⍵)} map ('^' mSpec seq ⍺⍺ sq ⍺⍺) ⍵}

   mForm←{
     p←mNum or mSym or mString
     p←p or (∇ mList)  or (∇ mVec)        or (∇ mMap)
     p←p or (∇ mQuote) or (∇ mQuasiquote) or (∇ mUnquoteOrSpliceUnquote)
     p←p or (∇ mDeref) or (∇mWithMeta)

     p ⍵
   }
   trim←{a←⍵=' ' ⋄ b←~(¯1↓(a,0)∧(1,a))∨(⌽∧\⌽a) ⋄ b/⍵}

   pprint←{
     t v←⍵
     t≡Number: trim⍕v
     t≡Symbol: v
     t≡String: '"',(unescape v),'"'
     t≡List:   '(',(trim⍕pprint¨ v),')'
     t≡Vec:    '[',(trim⍕pprint¨ v),']'
     t≡Map:    '{',(trim⍕pprint¨ v),'}'
     t≡Error:  'ERROR: ', v
     'error'                    ⍝ do something better than just returning a string 'error'
   }

   read←{
     s r R←tokens ⍵
     s: {
       s r R←mForm ⍵
       s: r
       Error 'end of input'
     }r
     Error 'Lexer error'
   }

   eval←{⍵}
   print←pprint
   rep←print∘eval∘read

   ∇R←StartMAL;inp;prompt;res;out;⎕TRAP
    prompt←'user> '
    :Trap 1004
      ⍞←prompt
      inp←(≢prompt)↓⍞
      →(inp≡'')/out
      →(inp≡'(exit)')/out
      res←rep inp
      ⍞←res
      ⍞←⎕ucs 10

      StartMAL
      →0
    :EndTrap
   out:'Bye'
   ∇
 :EndNamespace
