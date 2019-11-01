:Require file://debug.dyalog
:Require file://env.dyalog
:Require file://Types.dyalog
:Require file://Reader.dyalog
:Require file://Printer.dyalog
:Namespace m
  T←#.T
  Env←#.Env

  read←##.Reader.read

  mkPureFn←{(⍺⍺ ⍵) ⍺}           ⍝ call ⍺⍺ on ⍵, return ⍺ as env

  mkNumFn←{
    allNumbers←{∧/⊃¨#.T.Number=⍵}
    allNumbers ⍵: #.T.Number (⍺⍺ (⊃1∘↓)¨⍵)
    Error ⊂'Type Error'
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

  evDef←{
    x←⍺⍺
    (T.Error ('def! unimplemented.')) ⍺
  }
  evLet←{
    x←⍺⍺
    (T.Error ('let* unimplemented.')) ⍺
  }

  evLst←{
    h←lst.car ⍵
    _ t←lst.cdr ⍵
    h≡T.Symbol ('def!'): ⍺(⍺⍺evDef)⍵
    h≡T.Symbol ('let*'): ⍺(⍺⍺evLet)⍵
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
      (T.Error ('Name `',(2⊃⍵),''' is unbound.')) ⍺
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
    T.Error (⍕'Unknown type' ty)
  }

  print←##.Printer.pprint

  ∇R←env rep input
   v newEnv←env eval read input
   print v
   R←v newEnv
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
   ⎕off
  ∇
:EndNamespace
