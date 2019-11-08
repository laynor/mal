:Require file://Types.dyalog
:Namespace core
  T←#.Types
  list←T.List,⊂
  vec←T.Vec,⊂
  empty←list ⍬
  cons←{list ((⊂⍺),2⊃⍵)}
  car←{
    ⍵≡T.nil: T.nil
    ⊃(2⊃⍵),⊂T.nil
  }
  cdr←{
    T.nil≡⍵: empty
    list (⍬,1↓2⊃⍵)
  }
  last←{0=≢2⊃⍵: nil ⋄ ⊃¯1↑2⊃⍵}
  butlast←{0=≢2⊃⍵: nil ⋄ (⊃⍵) (¯1↓2⊃⍵)}
  concat←{
    list⊃,/2∘⊃¨#.m.SE ⍵
  }
  nth←{(⊂⍺)⌷2⊃⍵}
  fmap←{(⊃⍵) (⍺⍺¨2⊃⍵)}
:EndNamespace