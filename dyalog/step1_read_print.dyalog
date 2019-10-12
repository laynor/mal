 :Namespace p
   ⍝ Parsers take a stream of token and return three values:
   ⍝ success word rest ← parser 'my sentence'
   ⍝ success: 1 (success) or 0 (failure)

   ⍝ Coding convention: parsing result
   ⍝ s r R ← parser input
   ⍝ s: status, can be either Ok (1) or Fail (0)
   ⍝ r: result, any value in case of success, ⍬ in case of failure
   ⍝ R: rest, the remaining input to be parsed.

   ⍝ Documentation conventions:
   ⍝ P : a parser, that is, a function that takes a vector
   ⍝     of tokens and returns the triple (s r R)

   Ok←1
   fail←{0 (0⍴⍵) ⍵}
   eof←(0=≢),⍬∘,
   ∆t←{
     0=≢⍵: fail ⍵
     ⍺⍺ ⊃⍵: Ok (⊃⍵) (1↓⍵)
              fail ⍵
   }

   ⍝ implemented as operator so that it is correct to write
   ⍝ parser ← 'X' ∆k
   ∆k←{(=∘⍺⍺) ∆t ⍵}

   ⍝ f ∆map p : maps f on the result of p
   ∆map←{                   ⍝ ⍵⍵ is a parser function
     s r R←⍵⍵ ⍵            ⍝ ⍵ the usual array of tokens
     s (⍺⍺ r) R            ⍝ ⍺⍺ is a function that maps tokens to something else
   }

   ⍝ parser ∆many : builds a new parser that parses 0 or many occurrences of parser
   ∆many←{
     P←⍺⍺ ⋄ s r R1←⍺⍺ ⍵

     s: {                       ⍝ P success! parse many more of them
       s rs R2←P ∆many R1
       Ok  ((⊂r),rs) R2         ⍝ concat the result
     }⍬
     Ok r R1                    ⍝ P failed - return parsing results
   }

   ⍝ p1 ∆∆ p2 'my input' : applies p1 and p2 in sequence.
   ⍝                        returns an array of two elements
   ⍝                        containing the results of the two parsers
   ∆∆←{
     s1 r1 R1←⍺⍺ ⍵

     s1: r1 ∆map ⍵⍵ R1
     fail ⍵
   }

   ⍝ This expects its left argument to be a parser returning an array.
   ⍝ the results of the right parser are then added at the end of
   ⍝ the first parser results.
   ⍝ Example:
   ⍝ (⊂∆map symbol) ∆s number ∆s symbol
   ⍝ ^^^^^^^^^^^^^
   ⍝ Enclose the first of the sequence
   ∆s←{
     s r R←⍺⍺ ⍵

     s: (r,⊂) ∆map ⍵⍵ R
     fail ⍵
   }

   ⍝ parse one of more occurrences of ⍺⍺
   ∆some←{((⊃,/) ∆map ((⊂ ∆map ⍺⍺) ∆∆ (⍺⍺ ∆many))) ⍵}

   ⍝ parse either ⍺⍺ or ⍵⍵ - return the result of the first that matches
   ∆alt←{
     s r R←⍺⍺ ⍵

     s: s r R
        ⍵⍵ ⍵
   }

   ∇R←prn Y
    ⎕se.Dyalog.Utils.display Y
    R←Y
   ∇

   ⍝ same as alt, returns result of the parser that consumed more tokens (greedy)
   ∆or←{(1+>/(≢3∘⊃)¨R)⊃R←(⍺⍺ ⍵) (⍵⍵ ⍵)}

   ⍝ skip ⍺⍺
   skip←{(⊢∘(0⍴⍵)) ∆map ⍺⍺ ⍵}    ⍝ can't use yet, as sequencing operations die as
                                ⍝ a result of using this one.

   ⍝ Not always the right way to do it - gotta find the right definition
   ∆flat←{∊∆map ⍺⍺ ⍵}

   ⍝ Grammar
   ⍝ -------
   ∇importCombinators
    ##.∆t←∆t
    ##.∆k←∆k
    ##.∆∆←{(⍺⍺ p.∆∆ ⍵⍵) ⍵}
   ∇

   ⍝ Special chars
   NL←⎕ucs 13
   TAB←⎕ucs 9
   SPC←' '
   WS←TAB,SPC
   WSNL←NL,WS

   isAlpha←{⍵∊'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'}
   isDigit←{⍵∊'0123456789'}
   isAlphaNum←isAlpha∨isDigit

   ⍝ Lexing - basic chars
   digit←isDigit ∆t
   alpha←isAlpha ∆t
   alphaNum←isAlphaNum ∆t
   newline←NL ∆k
   isWhitespace1←∊∘WSNL
   isWhitespace←∧/∘isWhitespace1
   whitespace1←isWhitespace1 ∆t
   whitespace←whitespace1 ∆some ∆flat

   ⍝ MAL TOKENS
   ⍝ ----------

   ⍝ Strings

   ⍝ string : parse a string literal, escaped with the usual C rules.
   ⍝          returns the escaped string
   string←{
     dquote←'"' ∆k

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
       c='\': {(escape y),⍵} ∆map ∇ 2↓⍵
              {c,⍵} ∆map ∇ 1↓⍵
     }

     {⊃1↓⍵} ∆map (dquote ∆∆ stringRest) ⍵
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
     dquote←'"' ∆k

     stringRest←{
       c←1↑⍵
       cc←2↑⍵

       0=≢⍵: fail inp
       c='\': {cc,⍵} ∆map ∇ 2↓⍵
       c='"': Ok '"' (1↓⍵)
              {c,⍵} ∆map ∇ 1↓⍵
     }

     {∊⍵} ∆map (dquote ∆∆ stringRest) ⍵
   }

   semicolon←';'∆k
   specialChars←'''~@`()[]{}'
   specialCharsDqSemi←specialChars,'";,',WSNL
   symbolCharNotDigit←{~⍵∊specialCharsDqSemi,'0123456789.'}∆t
   symbolChar←{~⍵∊specialCharsDqSemi}∆t
   comma←(=∘',')∆t

   notNewLine←{~⍵∊NL}∆t
   comment←(semicolon ∆∆ (notNewLine ∆many) ∆s newline) ∆flat

   integer←digit ∆some
   symbol←∊ ∆map ((digit ∆many) ∆∆ symbolCharNotDigit ∆∆ (symbolChar ∆many))

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

   tok←whitespace ∆or comment ∆or comma
   tok←tok ∆or ({Special ⍵} ∆map special)
   tok←tok ∆or ({Number,toInt ⍵} ∆map integer)
   tok←tok ∆or ({Symbol ⍵} ∆map symbol)
   tok←tok ∆or ({String ⍵} ∆map string)
   tokens←(~isWhitespaceOrComment) flt ∆map (tok ∆many)

   mString←{String=tokType⍵} ∆t
   mNum←{Number=tokType⍵} ∆t
   mSym←{Symbol=tokType⍵} ∆t
   mSpec←{Special=tokType⍵} ∆t
   isSpecial←{ty val←⍺ ⋄ (ty=Special)∧(val≡⍵)}
   tSpec←{(isSpecial∘⍺⍺)∆t ⍵}

   ⍝ (spec applyToForm 'quote')
   applyToForm←{({List (Symbol s) (⊃1↓⍵)} ∆map ((⍵⍵ tSpec) ∆∆ ⍺⍺)) ⍵}

   mDelim←{{(⊃1↓⍵)} ∆map ((⊂∆map(⍺⍺[1] tSpec)) ∆s (⍵⍵ ∆many) ∆s (⍺⍺[2] tSpec)) ⍵}
   mList←{{List ⍵} ∆map ('()' mDelim ⍺⍺) ⍵}
   mVec←{{Vec ⍵} ∆map ('[]' mDelim ⍺⍺) ⍵}
   mMap←{{Map ⍵} ∆map ('{}' mDelim ⍺⍺) ⍵}
   specialHelper←{
     s r R←(((⊃⍵⍵) tSpec) ∆∆ ⍺⍺) ⍵
     s : Ok (List ((Symbol (1↓⍵⍵)) (⊃1↓r))) R
     fail ⍵
   }
   mQuote←{(⍺⍺ specialHelper '''quote') ⍵}
   ⍝ mUnquote←{(⍺⍺ specialHelper '~unquote')⍵}
   mQuasiquote←{(⍺⍺ specialHelper '`quasiquote')⍵}
   mDeref←{(⍺⍺ specialHelper '@deref')⍵}

   mUnquoteOrSpliceUnquote←{
     form←⍺⍺
     s1 r1 R1←('~' tSpec)⍵

     s1:{
       s2 r2 R2←('@' tSpec)R1
       s2: ({List ((Symbol 'splice-unquote') ⍵)} ∆map form) R2
       ({List ((Symbol 'unquote') ⍵)} ∆map form) R1
     }⍬

     fail ⍵
   }

   mForm←{mNum ∆or mSym ∆or mString ∆or (∇ mList) ∆or (∇ mVec) ∆or (∇ mMap) ∆or (∇ mUnquoteOrSpliceUnquote) ∆or (∇ mQuote)  ∆or (∇ mQuasiquote) ∆or (∇ mDeref) ⍵}
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
