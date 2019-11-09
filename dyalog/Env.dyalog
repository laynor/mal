:Require file://Types.dyalog
:Namespace Env
  T←#.Types

  empty←⊂0 2⍴'' 0

  ENV←1⍴empty
  PARENT←,1

  ∇R←new cur;id
   id←1+≢ENV
   PARENT←PARENT,cur
   ENV←ENV,empty
   R←id
  ∇

  ∇R←dispose id
   :if id=⍴ENV
     ENV←¯1↓ENV
   :endif
   R←⍴ENV
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
    i>≢ENV: T.nil
    (i⊃ENV)get1⍵
  }

  defn←{(⍺⍺ def (⍵⍵ T.mkBuiltin⍬)) ⍵}

  call←{f←⍺⍺ get ⍵⍵ ⋄ ⍵⍵ f.call ⍵}

:EndNamespace
