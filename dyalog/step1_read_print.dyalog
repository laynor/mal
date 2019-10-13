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
   digit←isDigit ∆t
   alpha←isAlpha ∆t
   alphaNum←isAlphaNum ∆t


   ⍝ Lexing - basic chars

   ⍝ Special chars
   NL←⎕ucs 13
   TAB←⎕ucs 9
   SPC←' '
   WS←TAB,SPC
   WSNL←NL,WS

   newline←NL only
   isWhitespace1←∊∘WSNL
   isWhitespace←∧/∘isWhitespace1
   whitespace1←isWhitespace1 ∆t
   whitespace←whitespace1 some flat

   ⍝ MAL TOKENS
   ⍝ ----------

   ⍝ Strings

   ⍝ string : parse a string literal, escaped with the usual C rules.
   ⍝          returns the escaped string
   string←{
     dquote←'"' only

     stringRest←{
       escape←{                   ⍝ we append ⍵ (the second char in the
         escapes←'trbn',⍵         ⍝ escape sequence) so that it is
         trans←(⎕ucs 9 10 8 13),⍵ ⍝ translated to itself when it's not a
         1↑(⍵=escapes)/trans      ⍝ known escape sequence
       }

       c←1↑⍵
       y←1↑1↓⍵

       0=≢⍵:  fail inp
       c='"': Ok '' (1↓⍵)
       c='\': (escape y)∘, map ∇ 2↓⍵
       {c,⍵} map ∇ 1↓⍵
     }

     {⊃1↓⍵} map (dquote seq stringRest) ⍵
   }

   unescape←{
     unescape1←{
       escapes←(('\'∘,)¨'trbn\"'),⍵
       trans←(⎕ucs 9 10 8 13),'\"',⍵
       1↑(⍵=trans)/escapes
     }
     ∊unescape1¨⍵
   }

   ⍝ stringNE : parse a string literal, return the string literal itself
   stringNE←{
     dquote←'"' only

     stringRest←{
       c←1↑⍵
       cc←2↑⍵

       0=≢⍵: fail inp
       c='\': ,⍨∘2↑⍵ map ∇ 2↓⍵
       c='"': Ok '"' (1↓⍵)
       {c,⍵} map ∇ 1↓⍵
     }

     {∊⍵} map (dquote seq stringRest) ⍵
   }

   semicolon←';'only
   specialChars←'''~@`()[]{}^'
   nonSymbol←specialChars,'";,',WSNL
   symbolCharNotDigit←{~⍵∊nonSymbol,'0123456789.'}∆t
   symbolChar←{~⍵∊nonSymbol}∆t
   comma←(=∘',')∆t

   notNewLine←{~⍵∊NL}∆t
   comment←(semicolon seq (notNewLine many) sq newline) flat

   integer←digit some
   symbol←∊ map ((digit many) seq symbolCharNotDigit seq (symbolChar many))

   special←{⍵∊specialChars} ∆t

   flt←{(⍺⍺¨⍵)/⍵}


   isComma←≡∘','
   isComment←';'≡⊃
   isWhitespaceOrComment←isWhitespace∨isComment∨isComma

   toInt←{0<≢⍵: ⍎⍵ ⋄ ⍵}

   ⍝ Tokens represented as a pair TokType value
   Special Symbol Number String List Vec Map Error←⍳8

   tokType←⊃
   tokVal←{1↑1↓⍵}

   tok←whitespace or comment or comma
   tok←tok or ({Special ⍵} map special)
   tok←tok or ({Number,toInt ⍵} map integer)
   tok←tok or ({Symbol ⍵} map symbol)
   tok←tok or ({String ⍵} map string)
   tokens←(~isWhitespaceOrComment) flt map (tok many)


   tt←{x←⍵ ⋄ {tt=tokType⍵}∆t}
   mString←{String=tokType⍵} ∆t
   mNum←{Number=tokType⍵} ∆t
   mSym←{Symbol=tokType⍵} ∆t
   mSpec←{Special=tokType⍵} ∆t
   isSpecial←{ty val←⍺ ⋄ (ty=Special)∧(val≡⍵)}
   tSpec←{(isSpecial∘⍺⍺)∆t ⍵}

   ⍝ In most of these parsers, ⍺⍺ is mForm. I don't know if there's a better way to
   ⍝ do mutual recursion

   ⍝ (spec applyToForm 'quote')
   applyToForm←{({List (Symbol s) (⊃1↓⍵)} map ((⍵⍵ tSpec) seq ⍺⍺)) ⍵}

   mDelim←{{(⊃1↓⍵)} map ((⊂map(⍺⍺[1] tSpec)) sq (⍵⍵ many) sq (⍺⍺[2] tSpec)) ⍵}

   mList←{{List ⍵} map ('()' mDelim ⍺⍺) ⍵}
   mVec←{{Vec ⍵} map ('[]' mDelim ⍺⍺) ⍵}
   mMap←{{Map ⍵} map ('{}' mDelim ⍺⍺) ⍵}
   specialHelper←{
     s r R←(((⊃⍵⍵) tSpec) seq ⍺⍺) ⍵
     s : Ok (List ((Symbol (1↓⍵⍵))(⊃1↓r))) R
     fail ⍵
   }
   mQuote←{(⍺⍺ specialHelper '''quote') ⍵}
   mQuasiquote←{(⍺⍺ specialHelper '`quasiquote')⍵}
   mDeref←{(⍺⍺ specialHelper '@deref')⍵}
   mUnquoteOrSpliceUnquote←{
     form←⍺⍺
     s1 r1 R1←('~' tSpec)⍵

     s1:{
       s2 r2 R2←('@' tSpec)R1
       s2: ({List ((Symbol 'splice-unquote') ⍵)} map form) R2
       ({List ((Symbol 'unquote') ⍵)} map form) R1
     }⍬
     fail ⍵
   }

   mWithMeta←{{List ((⊂Symbol 'with-meta'),⌽1↓⍵)} map ('^' tSpec seq ⍺⍺ sq ⍺⍺) ⍵}

   mForm←{
     p←mNum or mSym or mString
     p←p or (∇ mList) or (∇ mVec) or (∇ mMap)
     p←p or (∇ mQuote)  or (∇ mQuasiquote) or (∇ mUnquoteOrSpliceUnquote)
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
