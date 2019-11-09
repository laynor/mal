:Require file://Env.dyalog
:Require file://Errors.dyalog
:Require file://Printer.dyalog
:Require file://Reader.dyalog
:Require file://Types.dyalog
:Namespace core
  C E Env P R T←#.(Chars Errors Env Printer Reader Types)
  N S Str L V B nil empty←T.(Number Symbol String List Vec Bool nil empty)

  typeError←E.(TypeError∘throw)
  indexError←E.(IndexError∘throw)

  SE←{0=≢⍵: ⍵ ⋄ ⍺⍺ ⍵}           ⍝ safe each: return argument if emtpy

  eq←{B,⊃∧/2 T.eq/⍵}

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


  ⍝ strings
  str←{Str (⊃,/P.print¨⍵)}
  prStr←{Str (¯1↓⊃,/{(P.print_readably⍵),' '}¨SE ⍵)}

  ⍝ I/O
  prn←{⍞←(¯1↓⊃,/{(P.print_readably⍵),' '}¨⍵),C.LF ⋄ T.nil}
  println←{⍞←(¯1↓⊃,/{(P.print⍵),' '}¨⍵),C.LF ⋄ T.nil}
  slurp←{Str (⊃⎕nget 2⊃⊃⍵)}

  ⍝ Sexps
  readString←{R.read 2⊃⊃⍵}

  ⍝ seqs
  vec←{V,⊂⍵}
  list←{L,⊂⍵}
  first←{T.car⊃⍵}
  rest←{T.cdr⊃⍵}
  isList←{T.bool L=⊃⊃⍵}
  isEmpty←{ty v←⊃⍵ ⋄ T.bool (ty∊L V)∧(0=≢v)}
  cons←{(⊃⍵)T.cons 2⊃⍵}
  concat←{list⊃,/2∘⊃¨#.m.SE ⍵}
  count←{(2-S 'nil'≡⊃⍵)⊃(N 0) (N,≢2⊃⊃⍵)}
  last←{T.last⊃⍵}
  butlast←{T.butlast⊃⍵}
  fmap←{(⊃⍵) (⍺⍺¨2⊃⍵)}
  nth←{
    i←1+2⊃2⊃⍵
    i>≢2⊃⍵: indexError i
    ⊃i T.nth (concat (⊃⍵) (L (i⍴⊂nil)))
  }

  ⍝ Atoms
  atom←{T.newAtom⊃⍵}
  isAtom←{T.bool T.Atom≡⊃⊃⍵}
  deref←{T.deref⊃⍵}
  reset←{(⊃⍵) T.set (2⊃⍵)}



  ∆←{⎕this.⍺⍺ Env.fbind ,⍵}
  D←{1 2⍴⍵ ⍺}

  EX ←nil         D  'nil'
  EX⍪←plus        ∆  '+'
  EX⍪←minus       ∆  '-'
  EX⍪←multiply    ∆  '*'
  EX⍪←divide      ∆  '/'
  EX⍪←lt          ∆  '<'
  EX⍪←lte         ∆  '<='
  EX⍪←eq          ∆  '='
  EX⍪←gte         ∆  '>='
  EX⍪←gt          ∆  '>'
  EX⍪←atom        ∆  'atom'
  EX⍪←isAtom      ∆  'atom?'
  EX⍪←butlast     ∆  'butlast'
  EX⍪←first       ∆  'car'
  EX⍪←rest        ∆  'cdr'
  EX⍪←concat      ∆  'concat'
  EX⍪←cons        ∆  'cons'
  EX⍪←count       ∆  'count'
  EX⍪←deref       ∆  'deref'
  EX⍪←isEmpty     ∆  'empty?'
  EX⍪←first       ∆  'first'
  EX⍪←last        ∆  'last'
  EX⍪←list        ∆  'list'
  EX⍪←isList      ∆  'list?'
  EX⍪←nth         ∆  'nth'
  EX⍪←prStr       ∆  'pr-str'
  EX⍪←println     ∆  'println'
  EX⍪←prn         ∆  'prn'
  EX⍪←readString  ∆  'read-string'
  EX⍪←reset       ∆  'reset!'
  EX⍪←rest        ∆  'rest'
  EX⍪←slurp       ∆  'slurp'
  EX⍪←str         ∆  'str'

  EXPORTS←EX
:EndNamespace