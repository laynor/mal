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

  mkFn←{
    ns←⎕NS''
    ns.call←⍺⍺
    T.Function ns
  }

  def1←{((,⍺⍺) ⍵⍵)⍪⍵}
  in1←{(⊂,⍺)∊⍵[;1]}
  get1←{⊃⍺[⍺[;1]⍳⊂,⍵;2]}

  in←{∨/⍺∘in1¨⍵}
  ⍝ get←{(⊃(in1∘⍵¨⍺)/⍺) get1 ⍵}
  get←{(⊃⍺[(⍵∘in1¨⍺)⍳1]) get1 ⍵}
  def←{((⊂ ⍺⍺ def1 ⍵⍵)⊃⍵),1↓⍵}
  defn←{(⍺⍺ def (⍵⍵ mkFn⍬)) ⍵}

  call←{f←⍺⍺ get ⍵⍵ ⋄ ⍵⍵ f.call ⍵}

  ⍝ invece di fare il controllo "in" nella eval quando si cerca il simbolo, concatenare un valore posticcio all'ultima posizione dell'env
  ⍝ in modo tale che l'index resituisca quella posizione in caso di errore.
  ⍝ nel caso di una funzione, ad esempio, si potrebbe restituire una funzione che fa il raise del name error.
:EndNamespace
