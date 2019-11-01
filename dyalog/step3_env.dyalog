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

  defn←{(⍺⍺ Env.defn ⍵⍵) ⍵}
  defnp←{(⍺⍺ defn (⍵⍵ mkPureFn)) ⍵}
  defOp←{(⍺⍺ defnp (⍵⍵ mkNumFn)) ⍵}

  mkBaseEnv←{
    e←('+' defOp (+/))   Env.empty
    e←('-' defOp (⊃1∘↑-(+/1∘↓))) e
    e←('*' defOp (×/))           e
    e←('/' defOp (⊃1∘↑÷(×/1∘↓))) e
    e
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

  :Namespace lst
    L←##.T.List
    cons←{L ((⊂⍺),2⊃⍵)}
    car←{⊃2⊃⍵}
    cdr←{L (1↓2⊃⍵)}
  :EndNamespace

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
    val env1←⍺ ⍺⍺ form
    env2←(((2⊃name) Env.def val) env1)
    ⍝ ⎕←env2
    val env2
  }


  ⍝ TODO type check names
  evLet←{
    eval←⍺⍺
    (_ bs) exp←⍵                ⍝ TODO check type!
    bs←({⍺⍵}/(((⍴bs)÷2),2)⍴bs)  ⍝ group by 2
    env←Env.empty,⍺
    ⍝ Evaluate bindings
    env←⊃(eval evBinding)/(⌽bs),⊂env
    res _←env ⍺⍺ exp
    res ⍺
  }

  evLst←{
    h←lst.car ⍵
    _ t←lst.cdr ⍵
    h≡T.Symbol ('def!'): ⍺(⍺⍺evDef)t
    h≡T.Symbol ('let*'): ⍺(⍺⍺evLet)t
    ⍺(⍺⍺evFn)⍵
  }

  eval←{
    ty←⊃⍵
    ty≡T.Number: ⍵ ⍺
    ty≡T.String: ⍵ ⍺
    ty≡T.Function: ⍵ ⍺
    ty≡T.Error: ⍵ ⍺
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
    (ty≡T.List): ⍵ ⍺
    throw (⍕'Unknown type' ty)
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
