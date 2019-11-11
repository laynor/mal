:Require file://debug.dyalog
:Require file://Env.dyalog
:Require file://Types.dyalog
:Require file://Reader.dyalog
:Require file://Printer.dyalog
:Namespace m
  T←##.Types
  Env←##.Env

  read←##.Reader.read

  mkPureFn←{(⍺⍺ ⍵) ⍺}           ⍝ call ⍺⍺ on ⍵, return ⍺ as env

  allNumbers←{∧/⊃¨T.Number=⍵}
  mkNumFn←{
    #.m.allNumbers ⍵: T.Number (⍺⍺ (⊃1∘↓)¨⍵)
    Error ⊂'Type Error'
  }

  defn←{(⍺⍺ Env.defn ⍵⍵) ⍵}
  defnp←{(⍺⍺ defn (⍵⍵ mkPureFn)) ⍵}
  defOp←{(⍺⍺ defnp (⍵⍵ mkNumFn)) ⍵}

  GLOBAL←1
  mkBaseEnv←{
    e←('+' defOp (+/))          GLOBAL
    e←('-' defOp (⊃1∘↑-(+/1∘↓))) e
    e←('*' defOp (×/))          e
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

  evFn←{
    farg←⊃2⊃⍵
    args←1↓2⊃⍵
    eval←⍺⍺
    env←⍺
    (ty val) env1←env eval farg
    T.Builtin≠ty: (T.Error 'Type error') env1
    {
      args env2←env1 (eval vEach) args
      env2 val.call args
    }⍬
  }

  eval←{
    ty←⊃⍵
    ty≡T.Number: ⍵ ⍺
    ty≡T.String: ⍵ ⍺
    ty≡T.Builtin: ⍵ ⍺
    ty≡T.Error: ⍵ ⍺
    ty≡T.Keyword: ⍵ ⍺
    ty≡T.Symbol: ⍺{
      ':'=⊃2⊃⍵: ⍵ ⍺            ⍝ keywords
      (2⊃⍵) Env.in ⍺: ((⍺Env.get 2⊃⍵)) ⍺
      (T.Error ('Name `',(2⊃⍵),''' is unbound.')) ⍺
    }⍵
    ty≡T.Vec: ⍺{
      vs env←(⍺(eval vEach)2⊃⍵)
      (T.Vec vs) env
    }⍵
    ty≡T.Map: ⍺{
      vs env←⍺(eval vEach),2⊃⍵
      (T.Map (((0.5×⍴vs),2)⍴vs)) env
    }⍵
    (ty≡T.List)∧0<≢2⊃⍵: ⍺(∇evFn)⍵
    (ty≡T.List): ⍵ ⍺
    T.Error (⍕'Unknown type' ty)
  }

  print←##.Printer.print_readably

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
     res newEnv←GLOBAL rep inp
     ⍝ ⍞←res
     ⍞←⎕ucs 10

     StartMAL newEnv
     →0
   :EndTrap
  out:'Bye'
   ⍝ ⎕off
  ∇
:EndNamespace
