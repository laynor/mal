:Require file://Types.dyalog
:Namespace core
  E T←#.(Errors Types)
  N S L V B nil empty←T.(Number Symbol List Vec Bool nil empty)

  typeError←E.(TypeError∘throw)
  indexError←E.(IndexError∘throw)

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

  last←{T.last⊃⍵}
  butlast←{T.butlast⊃⍵}
  concat←{list⊃,/2∘⊃¨#.m.SE ⍵}
  fmap←{(⊃⍵) (⍺⍺¨2⊃⍵)}

  first←{T.car⊃⍵}
  rest←{T.cdr⊃⍵}
  car←T.car
  cdr←T.cdr
  nth←{
    i←1+2⊃2⊃⍵
    i>≢2⊃⍵: indexError i
    ⊃i T.nth (concat (⊃⍵) (L (i⍴⊂nil)))
  }
  cons←{(⊃⍵)T.cons 2⊃⍵}
:EndNamespace