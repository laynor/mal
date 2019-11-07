:Require file://Types.dyalog
:Namespace core
  list←#.T.List,⊂
  vec←#.T.Vec,⊂
  empty←list ⍬
  cons←{list ((⊂⍺),2⊃⍵)}
  car←{⊃2⊃⍵}
  cdr←{(⊃⍵) (1↓2⊃⍵)}
  last←{0=≢2⊃⍵: nil ⋄ ⊃¯1↑2⊃⍵}
  butlast←{0=≢2⊃⍵: nil ⋄ (⊃⍵) (¯1↓2⊃⍵)}
  concat←{list⊃,/2∘⊃¨⍵}
  nth←{⍺⊃2⊃⍵}
  fmap←{(⊃⍵) (⍺⍺¨2⊃⍵)}
:EndNamespace