 :Namespace c
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

   ⍝ parse eof (empty vector)
   eof←(0=≢),⍬∘,

   ⍝ pred terminal : scans and returns one token T if (pred T), fails otherwise
   terminal←{                   ⍝ ⍵ is an array of tokens, ⍺⍺ a predicate
     0=≢⍵:    Fail (0⍴⍵) ⍵
     ⍺⍺ ⊃1↑⍵: Ok   (⊃1↑⍵) (1↓⍵) ⍝ if (⍺⍺ ⍵[1]) [Ok w[1] ⍵[1..]]
              Fail (0⍴⍵) ⍵      ⍝ else fail empty ⍵
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

 :EndNamespace
