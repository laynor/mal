:Require file://Types.dyalog
:Require file://Errors.dyalog
:Namespace core
  typeError←#.E.(TypeError∘throw)
  arityError←#.E.(ArityError∘throw)
  N B L V A S Str←#.T.(Number Bool List Vec Atom Symbol String)

  nil←#.T.nil

  ⍝ expected checkTypes arglist
  checkTypes←{
    (≢⍺)≠≢⍵: arityError ⍺ ⍵ ⋄ ⍵
    ts←⊃¨⍵
    pairs→(⍪('*'@(⍸⍺≡¨'*'))ts)
    ∧/∊/cmp,⍪⍺: ⍵
    i←⍸≠/cmp
    ts[i] typeError ⍵[i]
  }

  mkNumFn←{
    nonNumber←(N=⊃¨⍵)⍳0
    nonNumber>⍴⍵: N (⍺⍺ (⊃1∘↓)¨⍵)
    typeError N (nonNumber⊃⍵)
  }

  mkRelFn←{
    nonNumber←(N=⊃¨⍵)⍳0
    nonNumber>⍴⍵: B (⍺⍺ (⊃1∘↓)¨⍵)
    typeError N (nonNumber⊃⍵)
  }

  ⍝ Err.throw (T.Number Err.typeError ⍵)
  ⍝ eval: ripensare a come esporla
  ⍝ (eval form) dovrebbe essere semplicemente trattato come GLOBAL eval form


  list←L,⊂
  vec←V,⊂
  empty←list ⍬
  cons←{
    h t←'*' 'LV' checkTypes ⍵

    list ((⊂h),2⊃t)
  }
  car←{
    ⍵≡nil: nil
    ⊃(2⊃⍵),⊂nil
  }
  cdr←{
    nil≡⍵: empty
    list (⍬,1↓2⊃⍵)
  }
  last←{0=≢2⊃⍵: nil ⋄ ⊃¯1↑2⊃⍵}
  butlast←{0=≢2⊃⍵: nil ⋄ (⊃⍵) (¯1↓2⊃⍵)}
  concat←{list⊃,/2∘⊃¨⍵}
  nth←{
    seq i←⍵
    ~(⊃seq)∊⍵
    (⊂⍺)⌷2⊃⍵
  }
  fmap←{(⊃⍵) (⍺⍺¨2⊃⍵)}
:EndNamespace