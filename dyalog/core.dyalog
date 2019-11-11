:Require file://Env.dyalog
:Require file://Errors.dyalog
:Require file://Printer.dyalog
:Require file://Reader.dyalog
:Require file://Types.dyalog
:Namespace core
  C E Env P R T←#.(Chars Errors Env Printer Reader Types)
  N S K Str L V M B Fun BFun←T.(Number Symbol Keyword String List Vec Map Bool Function Builtin)
  nil empty true false←T.(nil empty true false)
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

  ⍝ Type predicates
  EX⍪←(isNumber←{T.bool N=⊃⊃⍵})                                  ∆ 'number?'
  EX⍪←(isString←{T.bool Str=⊃⊃⍵})                                ∆ 'string?'
  EX⍪←(isSymbol←{T.bool S=⊃⊃⍵})                                  ∆ 'symbol?'
  EX⍪←(isKeyword←{T.bool K=⊃⊃⍵})                                 ∆ 'keyword?'
  EX⍪←(isSequential←{T.bool L V∊⍨⊃⊃⍵})                           ∆ 'sequential?'
  EX⍪←(isVec←{T.bool V=⊃⊃⍵})                                     ∆ 'vector?'
  EX⍪←(isList←{T.bool L=⊃⊃⍵})                                    ∆ 'list?'
  EX⍪←(isMap←{T.bool M=⊃⊃⍵})                                     ∆ 'map?'
  EX⍪←(isAtom←{T.bool T.Atom≡⊃⊃⍵})                               ∆ 'atom?'
  isFunction←{
    ~Fun BFun∊⍨⊃⊃⍵: false
    T.bool ~(2⊃⊃⍵).isMacro
  }
  EX⍪←isFunction                                                 ∆ 'fn?'
  isMacro←{
    Fun≠⊃⊃⍵: false
    T.bool (2⊃⊃⍵).isMacro
  }
  EX⍪←isMacro                                                    ∆ 'macro?'

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
  EX⍪←(symbol←{S(2⊃⊃⍵)})                                         ∆ 'symbol'

  ⍝ Keywords
  EX⍪←(keyword←{K (((~':'=⊃name)⍴':'),name) ⊣ name←2⊃⊃⍵})        ∆ 'keyword'

  EX⍪←(isNil←{T.bool nil≡⊃⍵})                                    ∆ 'nil?'
  EX⍪←(isTrue←{T.bool true≡⊃⍵})                                  ∆ 'true?'
  EX⍪←(isFalse←{T.bool false≡⊃⍵})                                ∆ 'false?'

  ⍝ strings
  EX⍪←(str←{Str (⊃,/P.print¨⍵)})                                 ∆ 'str'
  EX⍪←(prStr←{Str (¯1↓⊃,/{(P.print_readably⍵),' '}¨SE ⍵)})       ∆ 'pr-str'

  ⍝ I/O
  EX⍪←(prn←{⍞←(¯1↓⊃,/{(P.print_readably⍵),' '}¨⍵),C.LF ⋄ nil})   ∆ 'prn'
  EX⍪←(println←{⍞←(¯1↓⊃,/{(P.print⍵),' '}¨⍵),C.LF ⋄ nil})        ∆ 'println'
  EX⍪←(slurp←{Str (⊃⎕nget 2⊃⊃⍵)})                                ∆ 'slurp'
  readline←{
    1004:: nil
    ⍞←2⊃⊃⍵
    input←⍞
    input←(⍴2⊃⊃⍵)↓input
    Str input
  }
  EX⍪←readline                                                   ∆ 'readline'

  ⍝ Sexps
  EX⍪←(readString←{R.read 2⊃⊃⍵})                                 ∆ 'read-string'

  ⍝ seqs
  EX⍪←(vec←{V,⊂⍵})                                               ∆ 'vector'
  EX⍪←(list←{L,⊂⍵})                                              ∆ 'list'
  EX⍪←(first←{T.car⊃⍵})                                          ∆ 'first'
  EX⍪←(rest←{T.cdr⊃⍵})                                           ∆ 'rest'
  EX⍪←(isEmpty←{ty v←2↑⊃⍵ ⋄ T.bool (ty∊L V)∧(0=≢v)})             ∆ 'empty?'
  EX⍪←(cons←{(⊃⍵)T.cons 2⊃⍵})                                    ∆ 'cons'
  conj←{
    L≡⊃⊃⍵: L ((1↓⍵),2⊃⊃⍵)
    V≡⊃⊃⍵: V ((2⊃⊃⍵), 1↓⍵)
    typeError (L V) (⊃⍵)
  }
  EX⍪←conj                                                       ∆ 'conj'
  EX⍪←(concat←{list⊃,/2∘⊃¨#.m.SE ⍵})                             ∆ 'concat'
  EX⍪←(count←{(2-nil≡⊃⍵)⊃(N 0) (N,≢2⊃⊃⍵)})                   ∆ 'count'
  EX⍪←(last←{T.last⊃⍵})                                          ∆ 'last'
  EX⍪←(butlast←{T.butlast⊃⍵})                                    ∆ 'butlast'
  nth←{
    i←1+2⊃2⊃⍵
   (i<1)∨i>≢2⊃⊃⍵: indexError (N (¯1+i))
    ⊃i T.nth ⊃⍵
  }
  EX⍪←nth                                                        ∆ 'nth'

  ⍝ Map
  EX⍪←(assoc←{(⊃⍵)T.assoc 1↓⍵})                                  ∆ 'assoc'
  EX⍪←(dissoc←{⊃T.dissoc/⌽⍵})                                 ∆ 'dissoc'
  get←{
    3:: nil
    nil≡⊃⍵: nil
    (2⊃⍵)T.mapGet⊃⍵
  }
  EX⍪←get                                                        ∆ 'get'
  EX⍪←(hashMap←{assoc (⊂T.emptyMap),⍵})                          ∆ 'hash-map'
  EX⍪←(contains←{T.bool (2⊃⍵)T.mapIn⊃⍵})                         ∆ 'contains?'
  EX⍪←(keys←{L ((2⊃⊃⍵)[;1])})                                    ∆ 'keys'
  EX⍪←(vals←{L ((2⊃⊃⍵)[;2])})                                    ∆ 'vals'

  ⍝ Atoms
  EX⍪←(atom←{T.newAtom⊃⍵})                                       ∆ 'atom'
  EX⍪←(deref←{T.deref⊃⍵})                                        ∆ 'deref'
  EX⍪←(reset←{(⊃⍵) T.set (2⊃⍵)})                                 ∆ 'reset!'

  ⍝ Meta data
  EX⍪←(withMeta←{(2↑⊃⍵),⊂2⊃⍵})                                    ∆ 'with-meta'
  EX⍪←(meta←{3⊃(⊃⍵),⊂nil})                                       ∆ 'meta'



  EX⍪←(throw←{E.throw ⊃⍵})                                       ∆ 'throw'

  EXPORTS←EX
:EndNamespace