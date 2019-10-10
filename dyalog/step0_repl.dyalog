 :Namespace p
   read←{⍵}
   eval←{⍵}
   print←{⍵}
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
    out: 'Bye'
   ∇
 :EndNamespace
