 :Namespace p
   read←{⍵}
   eval←{⍵}
   print←{⍵}
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
