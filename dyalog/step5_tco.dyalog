:Require file://debug.dyalog
:Require file://env.dyalog
:Require file://Types.dyalog
:Require file://Reader.dyalog
:Require file://Printer.dyalog
:Require file://C.dyalog
:Namespace m
  T←#.T
  Env←#.Env
  C←#.C

  :Namespace E
    nameError←{'Name error: ''',⍵,''' not found.'}

    ty←{
      msg←#.('Type Error: expected ', (T.typeName ⍺), ', found ', T.typeName ⍵)
      #.T.Error msg
    }
  :EndNamespace


  read←##.Reader.read

  ⍝ Would really like to avoid having to fully qualify namespaces here
  mkNumFn←{
    N←#.T.Number
    nonNumber←(N=⊃¨⍵)⍳0
    nonNumber>⍴⍵: N (⍺⍺ (⊃1∘↓)¨⍵)
                  N #.m.E.ty (⊃nonNumber⊃⍵)
  }

  mkRelFn←{
    nonNumber←(#.T.Number=⊃¨⍵)⍳0
    nonNumber>⍴⍵: #.T.bool (⍺⍺ (⊃1∘↓)¨⍵)
    #.T.Number #.m.E.ty (⊃nonNumber⊃⍵)
  }

  defn←{(⍺⍺ Env.defn ⍵⍵) ⍵}
  defOp←{(⍺⍺ defn (⍵⍵ mkNumFn)) ⍵}
  defRelOp←{(⍺⍺ defn (⍵⍵ mkRelFn)) ⍵}

  nil←(read 'nil')

  :Namespace lst
    L←##.T.List
    empty←L ⍬
    cons←{L ((⊂⍺),2⊃⍵)}
    car←{⊃2⊃⍵}
    cdr←{L (1↓2⊃⍵)}
    list←{⊃cons/⍵,⊂empty}
  :EndNamespace

  pairwiseAll←{
    ∧/2⍺⍺/⍵
  }

  GLOBAL←1

  eq←{
    eqLst←{
      (≢⍺)≠≢⍵: 0
      ∧/#.m.eq/(⍪⍺),⍪⍵
    }
    ty1 v1←⍺
    ty2 v2←⍵
    ∧/ty1 ty2∊T.List T.Vec: (0,v1) eqLst (0,v2)
    ∧/ty1 ty2=T.Map:         v1 eqLst v2
                             ⍺≡⍵
  }

  mkBaseEnv←{
    e←GLOBAL
    _←('nil'     Env.def  nil) e
    _←('+'       defOp    (+/)) e
    _←('-'       defOp    (⊃1∘↑-(+/1∘↓))) e
    _←('*'       defOp    (×/))           e
    _←('/'       defOp    (⊃1∘↑÷(×/1∘↓))) e
    _←('>'       defRelOp {∧/ 2>/⍵}) e
    _←('<'       defRelOp {∧/ 2</⍵}) e
    _←('<='      defRelOp {∧/ 2≤/⍵}) e
    _←('>='      defRelOp {∧/ 2≥/⍵}) e
    _←('='       defn     {#.T.bool ⊃∧/ 2 #.m.eq/⍵}) e
    _←('list'    defn     {#.m.lst.list ⍵}) e
    _←('list'    defn     {#.m.lst.list ⍵}) e
    _←('list?'   defn     {T.bool #.T.List=⊃⊃⍵}) e
    _←('empty?'  defn     {ty v←⊃⍵ ⋄ T.bool (ty∊#.T.List #.T.Vec)∧(0=≢v)}) e
    _←('str'     defn     {#.T.String (⊃,/#.Printer.print¨⍵)})e
    _←('pr-str'  defn     {#.T.String (¯1↓⊃,/{(#.Printer.print_readably⍵),' '}¨⍵)})e
    _←('prn'     defn     {⎕←(¯1↓⊃,/{(#.Printer.print_readably⍵),' '}¨⍵) ⋄ #.T.nil})e
    _←('println' defn     {⎕←(¯1↓⊃,/{(#.Printer.print⍵),' '}¨⍵) ⋄ #.T.nil})e
    _←('count'   defn     {
      ty v←⊃⍵
      #.T.Symbol 'nil'≡⊃⍵: #.T.Number 0
                           #.T.Number (≢v)
    })e
    _←('envs' defn  {⎕←#.Env.ENV ⋄ #.T.nil}) e
    GLOBAL
  }

  BaseEnv←mkBaseEnv⍬

  evFn←{
    F←lst.car ⍵
    A←2⊃lst.cdr ⍵
    (ty f)←⍺ ⍺⍺ F
    ~ty∊T.Function T.Builtin: (T.Error 'Type error') ⍺
    ⍺ f.call ⍺∘⍺⍺¨A
  }

  ⍝ env (eval evBinding) (name form)
  evBinding←{
    name form←⍵
    val←⍺ ⍺⍺ form
    ((2⊃name) Env.def val) ⍺
  }

  ∇throw error
   error ⎕signal 100
  ∇

  ⍝ TODO check name is actually a symbol
  evDef←{
    name form←⍵
    val←⍺ ⍺⍺ form
    _←(((2⊃name) Env.def val) ⍺)
    val
  }

  SE←{0=≢⍵: ⍵ ⋄ ⍺⍺ ⍵}        ⍝ safe each: do not execute when empty vector

  ⍝ TODO type check names
  evLet←{
    eval←⍺⍺
    (_ bs) exp←⍵                ⍝ TODO check type!
    ⍝ B←((0.5×≢bs) 2⍴bs)
    ⍝ N←B[;1]                     ⍝ names
    ⍝ V←B[;2]                     ⍝ values
    bs←({⍺⍵}/(((⍴bs)÷2),2)⍴bs)  ⍝ group by 2
    env←Env.new⍺
    _←(env∘(eval evBinding))¨SE bs ⍝ Evaluate bindings
    env ⍺⍺ exp
  }

  ⍝ evDo←{⊃(⍺⍺{_ env←⍵⋄env ⍺⍺ ⍺})/(⌽⍵),⊂0 ⍺}
  evDo←{⊃¯1↑⍺∘⍺⍺¨⍵}
  evIf←{
    cond then else←3↑⍵,⊂nil
    v←⍺ ⍺⍺ cond
    ~(⊂v)∊nil T.false: ⍺ ⍺⍺ then
    ⍺ ⍺⍺ else
  }

  evFnStar←{
    env←⍺
    params exp←⍵
    eval←⍺⍺

    D←⎕ns''
    D.params←params
    D.env←env
    D.exp←exp

    fn←D{
      D←⍺⍺
      P←2⊃D.params
      (_ x) y←¯2↑P
      V←(1+x≡,'&')              ⍝ varargs?
      P←V⊃P ((¯2↓P),⊂y)         ⍝ param names
      A←V⊃⍵ (((¯1+⍴P)↑⍵),⊂#.m.lst.list (⊂#.T.Symbol 'list'),(¯1+⍴P)↓⍵) ⍝ actual args
      bs←{⍺⍵}/(⍪P),(⍪A)
      env←Env.new D.env
      _←env∘(eval evBinding)¨SE bs ⍝ Evaluate bindings
      val←env eval D.exp
      val
    }
    fn #.T.mkFn⍬
  }

  evLst←{
    h←lst.car ⍵
    _ t←lst.cdr ⍵
    h≡T.Symbol 'def!': ⍺(⍺⍺evDef)t
    h≡T.Symbol 'if':   ⍺(⍺⍺evIf)t
    h≡T.Symbol 'fn*':  ⍺(⍺⍺evFnStar)t
    ⍺(⍺⍺evFn)⍵
  }

  eval←{
    ty←⊃⍵

    ty≡T.Symbol: ⍺{
      ':'=⊃2⊃⍵: ⍵             ⍝ keywords
      (2⊃⍵) Env.in ⍺: (⍺Env.get(2⊃⍵))
      throw E.nameError 2⊃⍵
    }⍵

    ty≡T.Vec: T.Vec (⍺eval¨2⊃⍵)

    ty≡T.Map: T.Map (⍺eval¨2⊃⍵)

    (ty≢T.List): ⍵

    0=≢2⊃⍵: ⍵
    T.Symbol 'let*'≡lst.car⍵: ⍺{
      (_ bs) exp←1↓2⊃⍵                ⍝ TODO check type!
      bs←({⍺⍵}/(((⍴bs)÷2),2)⍴bs)  ⍝ group by 2
      env←Env.new⍺
      _←(env∘(eval evBinding))¨SE bs ⍝ Evaluate bindings
      env eval exp
    }⍵
    T.Symbol 'do'≡lst.car⍵: ⍺{
      forms←1↓2⊃⍵
      x←⍺∘eval¨ forms
      0=≢x: T.nil
      ⊃¯1↑x
    }⍵

    T.Symbol 'if'≡lst.car⍵: ⍺{
      cond then else←3↑1↓(2⊃⍵),⊂T.nil
      c←⍺eval cond
      ~(⊂c)∊nil T.false: ⍺eval then
      ⍺eval else
    }⍵

    0<≢2⊃⍵: ⍺(∇evLst)⍵

    ⍵
  }

  print←##.Printer.pprint


  ∇R←env rep input
   :Trap 100
     v←env eval read input
     print v
     R←v
   :Case 100
     ⎕←⎕dmx.EM
     R←(T.Symbol ,⊂'nil') env
   :EndTrap
  ∇

  init←{
    not ←'(def! not (fn* [o] (if o false true)))'
    _←GLOBAL eval (read not)
    ⍬
  }

  ∇R←repIO recur
   prompt←'user> '
   :Trap 1004

     :If recur≤0
       R←recur
     :Else
       ⍞←prompt
       inp←(≢prompt)↓⍞
       :Select inp
       :Case ''
         R←0
       :Case '(exit)'
         R←¯1
       :Else
         res←GLOBAL rep inp
         ⍞←C.LF
         R←recur+1
       :EndSelect
     :EndIf
   :Else
     R←¯2
   :EndTrap
  ∇

  ∇mapl
   ⎕←'MA(P)L 0.1'
   init⍬
   r←repIO⍣≡1
   'Bye'
   :If r<0
     ⎕off
   :EndIf
  ∇
:EndNamespace
