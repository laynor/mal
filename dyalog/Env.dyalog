:Require file://Types.dyalog
:Namespace Env
  ⍝ Environments implemented as binding tables
  ⍝ ┌→────────────────────────────────────────┐
  ⍝ ↓ ┌→───────────┐ ┌→─────────────────────┐ │
  ⍝ │ │dedicated-to│ │∇ #.Types.[Namespace] │ │
  ⍝ │ └────────────┘ └+─────────────────────┘ │
  ⍝ │ ┌→────┐        ┌→─────────────────────┐ │
  ⍝ │ │hello│        │∇ #.Types.[Namespace] │ │
  ⍝ │ └─────┘        └+─────────────────────┘ │
  ⍝ │ ┌→──────┐      ┌→─────────────────────┐ │
  ⍝ │ │unless2│      │∇ #.Types.[Namespace] │ │
  ⍝ │ └───────┘      └+─────────────────────┘ │
  ⍝ └∊────────────────────────────────────────┘

  T←#.Types

  empty←⊂0 2⍴'' 0

  ⍝ ENV is vector of binding tables (environments).
  ⍝ environment ids are index to the environments table.
  ENV←1⍴empty
  PARENT←,1

  fbind←{1 2⍴⍵ (⍺⍺T.mkBuiltin⍬)}

  ⍝ Takes a binding table and concatenates it to the current env
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

  defAll←{
    (⊃ENV[⍺])⍪←⍵
    ≢⊃ENV[⍺]
  }


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
