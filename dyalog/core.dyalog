:Require file://Env.dyalog
:Require file://Errors.dyalog
:Require file://Printer.dyalog
:Require file://Reader.dyalog
:Require file://Types.dyalog
:Namespace core
  C E Env P R T←#.(Chars Errors Env Printer Reader Types)
  N S Str L V B nil empty←T.(Number Symbol String List Vec Bool nil empty)

  ⍝ Export mechanism and syntax.
  ⍝ Syntactically, the file appears like a table: exported functions and
  ⍝ values on the left, name with wich they appear in the mal env on the
  ⍝ right.

  ∆←{⎕this.⍺⍺ Env.fbind ,⍵}
  D←{1 2⍴⍵ ⍺}

  typeError←E.(TypeError∘throw)
  indexError←E.(IndexError∘throw)
  ⍝ Operator for numerical functions
  NFn←{
    NaN←(N=⊃¨⍵)⍳0
    NaN>⍴⍵: N (⍺⍺ (⊃1∘↓)¨⍵)
    typeError N (NaN⊃⍵)
  }

  ⍝ Operator for relational functions
  RFn←{
    NaN←(N=⊃¨⍵)⍳0
    NaN>⍴⍵: T.bool (∧/ 2 ⍺⍺/(⊃1∘↓)¨⍵)
    typeError N (NaN⊃⍵)
  }


  SE←{0=≢⍵: ⍵ ⋄ ⍺⍺ ⍵}           ⍝ safe each: return argument if emtpy

  EX←nil                                                         D 'nil'
  EX⍪←(eq←{B,⊃∧/2 T.eq/⍵})                                       ∆ '='

  ⍝ numerical operators
  EX⍪←(add←(+/)          NFn)                                    ∆ '+'
  EX⍪←(sub←(⊃1∘↑-(+/1∘↓))NFn)                                    ∆ '-'
  EX⍪←(mul←(×/)          NFn)                                    ∆ '*'
  EX⍪←(div←(⊃1∘↑÷(×/1∘↓))NFn)                                    ∆ '/'

  ⍝ Relational operators

  EX⍪←( gt←>RFn)                                                 ∆ '>'
  EX⍪←(gte←≥RFn)                                                 ∆ '>='
  EX⍪←( lt←<RFn)                                                 ∆ '<'
  EX⍪←(lte←≤RFn)                                                 ∆ '<='

  ⍝ Symbols
  EX⍪←(isSymbol←{T.bool S=⊃⊃⍵})                                   ∆ 'symbol?'

  ⍝ strings
  EX⍪←(str←{Str (⊃,/P.print¨⍵)})                                 ∆ 'str'
  EX⍪←(prStr←{Str (¯1↓⊃,/{(P.print_readably⍵),' '}¨SE ⍵)})       ∆ 'pr-str'

  ⍝ I/O
  EX⍪←(prn←{⍞←(¯1↓⊃,/{(P.print_readably⍵),' '}¨⍵),C.LF ⋄ nil})   ∆ 'prn'
  EX⍪←(println←{⍞←(¯1↓⊃,/{(P.print⍵),' '}¨⍵),C.LF ⋄ nil})      ∆ 'println'
  EX⍪←(slurp←{Str (⊃⎕nget 2⊃⊃⍵)})                                ∆ 'slurp'

  ⍝ Sexps
  EX⍪←(readString←{R.read 2⊃⊃⍵})                                 ∆ 'read-string'

  ⍝ seqs
  EX⍪←(vec←{V,⊂⍵})                                               ∆ 'vec'
  EX⍪←(list←{L,⊂⍵})                                              ∆ 'list'
  EX⍪←(first←{T.car⊃⍵})                                          ∆ 'first'
  EX⍪←(rest←{T.cdr⊃⍵})                                           ∆ 'rest'
  EX⍪←(isList←{T.bool L=⊃⊃⍵})                                    ∆ 'list?'
  EX⍪←(isEmpty←{ty v←⊃⍵ ⋄ T.bool (ty∊L V)∧(0=≢v)})               ∆ 'empty?'
  EX⍪←(cons←{(⊃⍵)T.cons 2⊃⍵})                                    ∆ 'cons'
  EX⍪←(concat←{list⊃,/2∘⊃¨#.m.SE ⍵})                             ∆ 'concat'
  EX⍪←(count←{(2-S 'nil'≡⊃⍵)⊃(N 0) (N,≢2⊃⊃⍵)})                   ∆ 'count'
  EX⍪←(last←{T.last⊃⍵})                                          ∆ 'last'
  EX⍪←(butlast←{T.butlast⊃⍵})                                    ∆ 'butlast'
  nth←{
    i←1+2⊃2⊃⍵
   (i<1)∨i>≢2⊃⊃⍵: indexError (N (¯1+i))
    ⊃i T.nth ⊃⍵
  }
  EX⍪←nth                                                        ∆ 'nth'
  ⍝ Atoms
  EX⍪←(atom←{T.newAtom⊃⍵})                                       ∆ 'atom'
  EX⍪←(isAtom←{T.bool T.Atom≡⊃⊃⍵})                               ∆ 'atom?'
  EX⍪←(deref←{T.deref⊃⍵})                                        ∆ 'deref'
  EX⍪←(reset←{(⊃⍵) T.set (2⊃⍵)})                                 ∆ 'reset!'

  EX⍪←(throw←{E.throw ⊃⍵})                                       ∆ 'throw'

  EXPORTS←EX
:EndNamespace