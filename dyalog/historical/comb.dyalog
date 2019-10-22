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
   eof←(0=≢),(⊂0∘⍴),⊂
   term←{
     0=≢⍵: fail ⍵
     ⍺⍺ ⊃⍵: Ok (⊃⍵) (1↓⍵)
              fail ⍵
   }

   ⍝ implemented as operator so that it is correct to write
   ⍝ parser ← 'X' only
   only←{(=∘⍺⍺) term ⍵}

   ⍝ f map p : maps f on the result of p
   map←{                   ⍝ ⍵⍵ is a parser function
     s r R←⍵⍵ ⍵            ⍝ ⍵ the usual array of tokens
     s (⍺⍺ r) R            ⍝ ⍺⍺ is a function that maps tokens to something else
   }

   pipe←{⍵⍵ map ⍺⍺ ⍵}

   ⍝ parser many : builds a new parser that parses 0 or many occurrences of parser
   many←{
     P←⍺⍺ ⋄ s r R1←⍺⍺ ⍵

     s: {                       ⍝ P success! parse many more of them
       s rs R2←P many R1
       Ok  ((⊂r),rs) R2         ⍝ concat the result
     }⍬
     Ok r R1                    ⍝ P failed - return parsing results
   }

   ⍝ p1 seq p2 'my input' : applies p1 and p2 in sequence.
   ⍝                        returns an array of two elements
   ⍝                        containing the results of the two parsers
   seq←{
     s1 r1 R1←⍺⍺ ⍵

     s1: r1 map ⍵⍵ R1
     fail ⍵
   }

   ⍝ This expects its left argument to be a parser returning an array.
   ⍝ the results of the right parser are then added at the end of
   ⍝ the first parser results.
   ⍝ Example:
   ⍝ (⊂map symbol) ∆ number ∆ symbol
   ⍝ ^^^^^^^^^^^^^
   ⍝ Enclose the first of the sequence
   ∆←{
     s r R←⍺⍺ ⍵

     s: (r,⊂) map ⍵⍵ R
     fail ⍵
   }

   ⍝ parse one of more occurrences of ⍺⍺
   some←{((⊃,/) map ((⊂ map ⍺⍺) seq (⍺⍺ many))) ⍵}

   ⍝ parse either ⍺⍺ or ⍵⍵ - return the result of the first that matches
   alt←{
     s r R←⍺⍺ ⍵

     s: s r R
        ⍵⍵ ⍵
   }

   ∇R←prn Y
    ⎕se.Dyalog.Utils.display Y
    R←Y
   ∇

   ⍝ same as alt, returns result of the parser that consumed more tokens (greedy)
   or←{(1+>/(≢3∘⊃)¨R)⊃R←(⍺⍺ ⍵) (⍵⍵ ⍵)}

   ⍝ skip ⍺⍺
   skip←{(⊢∘(0⍴⍵)) map ⍺⍺ ⍵}    ⍝ can't use yet, as sequencing operations die as
                                ⍝ a result of using this one.

   ⍝ Not always the right way to do it - gotta find the right definition
   flat←{∊map ⍺⍺ ⍵}
 :EndNamespace
