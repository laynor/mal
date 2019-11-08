:Require file://Types.dyalog
:Namespace core
  E T←#.(Errors Types)
  N S L V B nil←T.(Number Symbol List Vec Bool nil)

  typeError←E.TypeError∘E.throw

  mkNumFn←{
    NaN←(N=⊃¨⍵)⍳0
    NaN>⍴⍵: N (⍺⍺ (⊃1∘↓)¨⍵)
    typeError N (NaN⊃⍵)
  }

  mkRelFn←{

    NaN←(N=⊃¨⍵)⍳0
    NaN>⍴⍵: T.bool (⍺⍺ (⊃1∘↓)¨⍵)
    typeError N (NaN⊃⍵)
  }


  ⍝ Seqs
  vec←V,⊂
  list←L,⊂

  empty←L⍬

  cons←{list (⊂⍺),2⊃⍵}

  car←{
    ⍵≡nil: nil
    ⊃(2⊃⍵),⊂nil
  }

  cdr←{
    nil≡⍵: empty
    list (⍬,1↓2⊃⍵)
  }

  eq←{B,⊃∧/2 T.eq/⍵}

  ⍝ numerical operators
  plus←(+/)mkNumFn
  minus←(⊃1∘↑-(+/1∘↓))mkNumFn
  multiply←(×/)mkNumFn
  divide←(⊃1∘↑÷(×/1∘↓))mkNumFn

  ⍝ Relational operators
  gt←{∧/ 2>/⍵}mkRelFn
  gte←{∧/ 2≥/⍵}mkRelFn
  lt←{∧/ 2</⍵}mkRelFn
  lte←{∧/ 2≤/⍵}mkRelFn

  last←{0=≢2⊃⍵: nil ⋄ ⊃¯1↑2⊃⍵}
  butlast←{0=≢2⊃⍵: nil ⋄ (⊃⍵) (¯1↓2⊃⍵)}
  concat←{list⊃,/2∘⊃¨#.m.SE ⍵}
  nth←{(⊂⍺)⌷2⊃⍵}
  fmap←{(⊃⍵) (⍺⍺¨2⊃⍵)}

:EndNamespace