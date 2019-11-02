:Require file://Types.dyalog
:Namespace Env
  T←##.T

  ⍝ ∇R←(name defn fn) env
  ⍝  key←,name
  ⍝  value←T.Function (ns←⎕NS'')
  ⍝  ⎕CS ns
  ⍝  call←fn
  ⍝  R←(⊂key value)⍪env
  ⍝ ∇

  empty←⊂0 2⍴'' 0

  ENV←1⍴empty
  PARENT←,1

  ∇R←new cur;id
   id←1+≢ENV
   PARENT←PARENT,cur
   ENV←ENV,empty
   R←id
  ∇

  ∇R←(name def val) id
   def1←{((,⍺⍺) ⍵⍵)⍪⍵}
   env←id⊃ENV
   env2←(name def1 val) env
   ENV[id]←⊂env2
   R←id
  ∇

  idx←{
    in1←{(⊂,⍺)∊⍵[;1]}
    chain←{∪⍵,PARENT[⍵]}⍣≡⍵
    envs←ENV[chain]
    ⊃(chain,1+⍴ENV)[(⍺∘in1¨envs)⍳1]
  }

  in←{
    i←⍺idx⍵
    i≤⍴ENV
  }

  get←{
    get1←{⊃⍺[⍺[;1]⍳⊂,⍵;2]}
    i←⍵idx⍺
    (i⊃ENV)get1⍵
  }


  ⍝ in←{∨/⍺∘in1¨⍵}
  ⍝ get←{(⊃(in1∘⍵¨⍺)/⍺) get1 ⍵}
  ⍝ get←{(⊃⍺[(⍵∘in1¨⍺)⍳1]) get1 ⍵}
  ⍝ def←{((⊂ ⍺⍺ def1 ⍵⍵)⊃⍵),1↓⍵}
  defn←{(⍺⍺ def (⍵⍵ T.mkFn⍬)) ⍵}

  call←{f←⍺⍺ get ⍵⍵ ⋄ ⍵⍵ f.call ⍵}

  ⍝ invece di fare il controllo "in" nella eval quando si cerca il simbolo, concatenare un valore posticcio all'ultima posizione dell'env
  ⍝ in modo tale che l'index resituisca quella posizione in caso di errore.
  ⍝ nel caso di una funzione, ad esempio, si potrebbe restituire una funzione che fa il raise del name error.

:EndNamespace
