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
   Fail←0

   ⍝ true if ⍵ is an empty array
   empty←0≡≢

   ⍝ parse eof
   ⍝ eof←{(empty ⍵) ⍬ ⍵}
   eof←empty,⍬∘,


   ⍝ pred terminal : scans and returns one token T if (pred T), fails otherwise
   terminal←{                   ⍝ ⍵ is an array of tokens, ⍺⍺ a predicate
     empty ⍵: Fail (0⍴⍵) ⍵
     ⍺⍺ ⊃1↑⍵: Ok (⊃1↑⍵) (1↓⍵)    ⍝ if (⍺⍺ ⍵[1]) [Ok w[1] ⍵[1..]]
              Fail (0⍴⍵) ⍵          ⍝ else fail empty ⍵
   }

   ⍝ implemented as operator so that it is correct to write
   ⍝ parser ← 'X' tok
   tok←{(=∘⍺⍺) terminal ⍵}

   ⍝ f map p : maps f on the result of p
   map←{                        ⍝ ⍵⍵ is a parser function
     s r R←⍵⍵ ⍵                 ⍝ ⍵ the usual array of tokens
     s (⍺⍺ r) R                 ⍝ ⍺⍺ is a function that maps tokens to something else
   }

   ⍝ x cmap parser : shorthand for {x} map parser
   cmap←{
     x←⍺⍺
     {x} map ⍵⍵ ⍵
   }

   ⍝ parser many : builds a new parser that parses 0 or many occurrences of parser
   many←{
     P←⍺⍺                       ⍝ parser
     s r R1←⍺⍺ ⍵                ⍝ run the parser

     s: {                       ⍝ P success! parse many more of them
       s rs R2←P many R1
       Ok ((⊂r),rs) R2             ⍝ concat the result
     }⍬
     Ok r R1                    ⍝ P failed - return parsing results
   }

   ⍝ p1 seq p2 'my input' : applies p1 and p2 in sequence.
   ⍝                        returns an array of two elements
   ⍝                        containing the results of the two parsers
   seq←{
     p1←⍺⍺
     p2←⍵⍵
     inp←⍵
     s1 r1 R1←p1 inp

     s1:{
       s2 r2 R2←p2 R1

       s2: Ok (r1 r2) R2
           Fail (0⍴inp) inp
     }⍬
     Fail (0⍴inp) inp
   }

   ⍝ This expects its left argument to be a parser returning an array.
   ⍝ the results of the right parser are then added at the end of
   ⍝ the first parser results.
   ⍝ Example:
   ⍝ (⊂map symbol) seq2 number seq2 symbol
   ⍝ ^^^^^^^^^^^^^
   ⍝ Enclose the first of the sequence
   seq2←{
     p1←⍺⍺
     p2←⍵⍵
     inp←⍵
     s1 r1 R1←p1 inp

     s1:{
       s2 r2 R2←p2 R1

       s2: Ok (r1,⊂r2) R2
       Fail (0⍴inp) inp
     }⍬
     Fail (0⍴inp) inp
   }

   ⍝ parse one of more occurrences of ⍺⍺
   some←{((⊃,/) map ((⊂ map ⍺⍺) seq (⍺⍺ many))) ⍵}

   ⍝ parse either ⍺⍺ or ⍵⍵ - return the result of the first that matches
   alt←{
     s r R←⍺⍺ ⍵

     s: s r R
        ⍵⍵ ⍵
   }

   ⍝ same as alt, returns result of the parser that consumed more tokens (greedy)
   or←{
     s1 r1 R1←⍺⍺ ⍵
     s2 r2 R2←⍵⍵ ⍵
     (≢R1)≤≢R2: s1 r1 R1
                s2 r2 R2
   }

   ⍝ skip ⍺⍺
   skip←{{⍬} map ⍺⍺ ⍵}

   ⍝ Not always the right way to do it - gotta find the right definition
   flat←{∊map ⍺⍺ ⍵}

   ⍝ Grammar
   ⍝ -------

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
   digit←isDigit terminal
   alpha←isAlpha terminal
   alphaNum←isAlphaNum terminal
   newline←NL tok
   isWhitespace1←∊∘WSNL
   isWhitespace←∧/∘isWhitespace1
   whitespace1←isWhitespace1 terminal
   whitespace←whitespace1 some flat

   ⍝ terminals
   ⍝ ---------

   ⍝ Strings

   ⍝ string : parse a string literal, escaped with the usual C rules.
   ⍝          returns the escaped string
   string←{
     dquote←'"' tok

     stringRest←{
       escape←{
         ⍝ we append ⍵ (the second char in the escape sequence) so that it is
         ⍝ translated to itself when it's not a known escape sequence
         escapes←'trbn',⍵
         trans←(⎕ucs 9 10 8 13),⍵
         1↑(⍵=escapes)/trans
       }

       c←1↑⍵
       y←1↑1↓⍵

       empty ⍵: Fail ⍬ ⍵
       c='"':   Ok '' (1↓⍵)
       c='\':   {(escape y),⍵} map ∇ 2↓⍵
                {c,⍵} map ∇ 1↓⍵
     }

     {⊃1↓⍵} map (dquote seq stringRest) ⍵
   }


   ⍝ stringNE : parse a string literal, return the string literal itself
   stringNE←{
     dquote←'"' tok

     stringRest←{
       c←1↑⍵
       cc←2↑⍵

       empty ⍵: Fail ⍬ ⍵
       c='\':   {cc,⍵} map ∇ 2↓⍵
       c='"':   Ok '"' (1↓⍵)
                {c,⍵} map ∇ 1↓⍵
     }

     {∊⍵} map (dquote seq stringRest) ⍵
   }

   ⍝ quote      ← '''' tok
   ⍝ unquote    ← '~'  tok
   ⍝ deref      ← '@'  tok
   ⍝ quasiquote ← '`'  tok
   ⍝ openParen  ← '('  tok
   semicolon  ← ';'  tok
   specialChars←'''~@`()[]{}'
   specialCharsDqSemi←specialChars,'";',WSNL
   symbolCharNotDigit←{~⍵∊specialCharsDqSemi,'0123456789.'}terminal
   symbolChar←{~⍵∊specialCharsDqSemi}terminal

   notNewLine←{~⍵∊NL}terminal
   comment←(semicolon seq (notNewLine many) seq newline) flat

   integer←digit some flat
   symbol←∊ map ((digit many) seq symbolCharNotDigit seq (symbolChar many))

   special←{⍵∊specialChars} terminal

   flt←{(⍺⍺¨⍵)/⍵}

   Special Symbol Number String List Vec Map Error←⍳8

   isComment←{(1↑⍵)≡';'}
   isWhitespaceOrComment←isWhitespace∨isComment

   toInt←{
     (≢⍵)>0: ⍎⍵
             ⍵
   }

   token←(whitespace or ({Special ⍵} map special) or ({Number,toInt ⍵} map integer) or ({Symbol ⍵} map symbol) or ({String ⍵} map string) or comment)
   tokens←(~isWhitespaceOrComment) flt map (token many)

   tokType←1∘↑
   tokVal←{1↑1↓⍵}

   mString←{String=tokType⍵} terminal
   mNum←{Number=tokType⍵} terminal
   mSym←{Symbol=tokType⍵} terminal
   mSpec←{Special=tokType⍵} terminal
   isSpecial←{
     ty val←⍺
     (ty=Special)∧(val≡⍵)
   }
   tSpec←{(isSpecial∘⍺⍺)terminal ⍵}
   ⍝ all those disclose/take/drop ops are not very readable - that's because of
   ⍝ how the seq operator is implemented.
   ⍝ the seq operator returns an array of 2 boxes, containing the result of the
   ⍝ two sequenced parsers.
   mDelim←{{(⊃1↑1↓⍵)} map ((⊂map(⍺⍺[1] tSpec)) seq2 (⍵⍵ many) seq2 (⍺⍺[2] tSpec)) ⍵}
   mList←{{List ⍵} map ('()' mDelim ⍺⍺) ⍵}
   mVec←{{Vec ⍵} map ('[]' mDelim ⍺⍺) ⍵}
   mMap←{{Map ⍵} map ('{}' mDelim ⍺⍺) ⍵}
   mForm←{mNum or mSym or mString or (∇ mList) or (∇ mVec) or (∇ mMap) ⍵}
   trim←{a←⍵=' ' ⋄ b←~(¯1↓(a,0)∧(1,a))∨(⌽∧\⌽a) ⋄ b/⍵}

   pprint←{
     repr←{
       ⍵
     }
     t v←⍵
     t≡Number: trim⍕v
     t≡Symbol: v
     t≡String: '"',repr v,'"'
     t≡List:   '(',(trim⍕pprint¨ v),')'
     t≡Vec:    '[',(trim⍕pprint¨ v),']'
     t≡Map:    '{',(trim⍕pprint¨ v),'}'
     t≡Error:  'ERROR: ', v
     'error'                    ⍝ do something better than just returning a string 'error'
   }

   ⍝ untyped lambda calculus tokenizer
   var←(alpha seq (alphaNum many))  flat
   special←{⍵∊'.()λ'}terminal
   ulcTokens←(~isWhitespace) flt map ((var or special or whitespace)many)
   read←{
     s r R←tokens ⍵
     s: {
       s r R←mForm ⍵
       s: r
       Error 'Parse error'
     }r
     Error 'Lexer error'
   }
   eval←{⍵}
   print←pprint
   rep←print∘eval∘read

   ∇R←StartMAL;inp;prompt;res;out
    prompt←'user> '
    ⍞←prompt
    inp←(≢prompt)↓⍞
    →(inp≡'(exit)')/out
    res←rep inp
    ⎕←res
    StartMAL
    →0
    out: 'Bye'
   ∇
 :EndNamespace
