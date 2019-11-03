:Require file://debug.dyalog
:Require file://env.dyalog
:Require file://Types.dyalog
:Require file://Reader.dyalog
:Require file://Printer.dyalog
:Namespace m
  T←#.T
  Env←#.Env

  :Namespace E
    nameError←{'Name error: ''',⍵,''' not found.'}

    ty←{
      msg←#.('Type Error: expected ', (T.typeName ⍺), ', found ', T.typeName ⍵)
      #.T.Error msg
    }
  :EndNamespace


  read←##.Reader.read

  mkPureFn←{(⍺⍺ ⍵) ⍺}           ⍝ call ⍺⍺ on ⍵, return ⍺ as env


  ⍝ Would really like to avoid having to fully qualify namespaces here
  mkNumFn←{
    nonNumber←(#.T.Number=⊃¨⍵)⍳0
    nonNumber>⍴⍵: #.T.Number (⍺⍺ (⊃1∘↓)¨⍵)
                  #.T.Number #.m.E.ty (⊃nonNumber⊃⍵)
  }

  mkRelFn←{
    nonNumber←(#.T.Number=⊃¨⍵)⍳0
    nonNumber>⍴⍵: #.T.bool (⍺⍺ (⊃1∘↓)¨⍵)
    #.T.Number #.m.E.ty (⊃nonNumber⊃⍵)
  }

  defn←{(⍺⍺ Env.defn ⍵⍵) ⍵}
  defnp←{(⍺⍺ defn (⍵⍵ mkPureFn)) ⍵}
  defOp←{(⍺⍺ defnp (⍵⍵ mkNumFn)) ⍵}
  defRelOp←{(⍺⍺ defnp (⍵⍵ mkRelFn)) ⍵}

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
    ∧/ty1 ty2∊T.List T.Vec: v1 eqLst v2
    ∧/ty1 ty2=T.Map: v1 eqLst v2
    ⍺≡⍵
  }

  mkBaseEnv←{
    e←GLOBAL
    _←('+' defOp (+/)) e
    _←('-' defOp (⊃1∘↑-(+/1∘↓))) e
    _←('*' defOp (×/))           e
    _←('/' defOp (⊃1∘↑÷(×/1∘↓))) e
    _←('>'  defRelOp {∧/ 2>/⍵}) e
    _←('<'  defRelOp {∧/ 2</⍵}) e
    _←('<=' defRelOp {∧/ 2≤/⍵}) e
    _←('>=' defRelOp {∧/ 2≥/⍵}) e
    _←('='  defnp {#.T.bool ∧/ 2 #.m.eq/⍵}) e
    _←('not' defnp {
      #.T.nil≡⊃⍵: #.T.true
      #.T.false≡⊃⍵: #.T.true
      #.T.false
    }) e
    _←('list' defnp {#.m.lst.list ⍵}) e
    _←('list' defnp {#.m.lst.list ⍵}) e
    _←('list?' defnp {
      ty v←⊃⍵
      T.bool (ty=#.T.List)
    }) e
    _←('empty?' defnp {
      ty v←⊃⍵
      T.bool (ty=#.T.List)∧(0=≢v)
    }) e
    _←('count' defnp {
      ty v←⊃⍵
      (⊃⍵)≡#.T.Symbol 'nil': #.T.Number 0
      #.T.Number (≢v)
    })e
    _←('prn' defnp {
      ⎕←#.m.print ⊃⍵
      #.T.nil
    })e
    _←('envs' defnp {⎕←#.Env.ENV⋄#.T.nil}) e
    _←('nil' Env.def nil) e
    GLOBAL
  }

  BaseEnv←mkBaseEnv⍬

  vEach←{
    env←⍺
    vec←⍵
    eval←⍺⍺
    0=≢⍵:⍬ ⍺
    {
      v e1←env eval ⊃vec
      vs e2←env (eval vEach) (1↓vec)
      ((⊂v),vs) e2
    }⍬
  }

  evFn←{
    farg←lst.car ⍵
    args←2⊃lst.cdr ⍵
    eval←⍺⍺
    env←⍺
    (ty val) env1←env eval farg
    T.Function≠ty: (T.Error 'Type error') env1
    {
      args env2←env1 (eval vEach) args
      env2 val.call args
    }⍬
  }

  ⍝ env (eval evBinding) (name form)
  evBinding←{
    name form←⍺
    val env1←⍵ ⍺⍺ form
    (((2⊃name) Env.def val) env1)
  }

  ∇throw error
   error ⎕signal 100
  ∇

  ⍝ TODO check name is actually a symbol
  evDef←{
    name form←⍵
    val _←⍺ ⍺⍺ form
    _←(((2⊃name) Env.def val) ⍺)
    ⍝ ⎕←env2
    val ⍺
  }


  ⍝ TODO type check names
  evLet←{
    eval←⍺⍺
    (_ bs) exp←⍵                ⍝ TODO check type!
    bs←({⍺⍵}/(((⍴bs)÷2),2)⍴bs)  ⍝ group by 2
    env←Env.new⍺
    env←⊃(eval evBinding)/(⌽bs),⊂env ⍝ Evaluate bindings
    res _←env ⍺⍺ exp
    res ⍺
  }

  evDo←{⊃(⍺⍺{_ env←⍵⋄env ⍺⍺ ⍺})/(⌽⍵),⊂0 ⍺}
  evIf←{
    cond then else←3↑⍵,⊂nil
    v env1←⍺ ⍺⍺ cond
    ~(⊂v)∊nil T.false: env1 ⍺⍺ then
    env1 ⍺⍺ else
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
      ⍝ irest←((2∘⊃)¨2⊃D.params)⍳⊂,'&'
      ⍝ ⎕←(1+irest)⊃2⊃D.params,⍬⍬
      params←2⊃D.params
      (_ x) y←¯2↑params
      varargs←(1+x≡,'&')
      params←varargs⊃params ((¯2↓params),⊂y)
      args←varargs⊃⍵ (((¯1+⍴params)↑⍵),⊂#.m.lst.list (⊂#.T.Symbol 'list'),(¯1+⍴params)↓⍵)
      bs←{⍺⍵}/(⍪params),(⍪args)
      env←Env.new D.env
      _←(eval evBinding)/(⌽bs),⊂env ⍝ Evaluate bindings
      val _←env eval D.exp
      val ⍺
    }
    val←fn #.T.mkFn⍬
    val env
  }

  evLst←{
    h←lst.car ⍵
    _ t←lst.cdr ⍵
    h≡T.Symbol ('def!'): ⍺(⍺⍺evDef)t
    h≡T.Symbol ('let*'): ⍺(⍺⍺evLet)t
    h≡T.Symbol ('do'): ⍺(⍺⍺evDo)t
    h≡T.Symbol ('if'): ⍺(⍺⍺evIf)t
    h≡T.Symbol ('fn*'): ⍺(⍺⍺evFnStar)t
    ⍺(⍺⍺evFn)⍵
  }

  eval←{
    ty←⊃⍵

    ty≡T.Symbol: ⍺{
      ':'=⊃2⊃⍵: ⍵ ⍺             ⍝ keywords
      (2⊃⍵) Env.in ⍺: (⍺Env.get(2⊃⍵)) ⍺
      throw E.nameError 2⊃⍵
    }⍵

    ty≡T.Vec: ⍺{
      vs env←(⍺(eval vEach)2⊃⍵)
      (T.Vec vs) env
    }⍵

    ty≡T.Map: ⍺{
      vs env←(⍺(eval vEach)2⊃⍵)
      (T.Map vs) env
    }⍵

    (ty≡T.List)∧0<≢2⊃⍵: ⍺(∇evLst)⍵

    ⍵ ⍺
  }

  print←##.Printer.pprint


  ∇R←env rep input
   :Trap 100
     v newEnv←env eval read input
     print v
     R←v newEnv
   :Case 100
     ⎕←⎕dmx.EM
     R←(T.Symbol ,⊂'nil') env
   :EndTrap
  ∇

  ∇R←StartMAL env;inp;prompt;res;out;⎕TRAP
   env←(1+0=≢env)⊃env BaseEnv
   prompt←'user> '
   :Trap 1004
     ⍞←prompt
     inp←(≢prompt)↓⍞
     →(inp≡'')/out
     →(inp≡'(exit)')/out
     res newEnv←env rep inp
     ⍝ ⍞←res
     ⍞←⎕ucs 10

     StartMAL newEnv
     →0
   :EndTrap
  out:'Bye'
   ⍝ ⎕off
  ∇
:EndNamespace
