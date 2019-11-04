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
  ARGV←⍬

  :Namespace E
    nameError←{'Name error: ''',⍵,''' not found.'}

    ty←{
      msg←#.('Type Error: expected ', (T.typeName ⍺), ', found ', (⍕⍵), ':', (T.typeName ⊃⍵))
      #.m.throw msg
    }
  :EndNamespace


  read←##.Reader.read

  ⍝ Would really like to avoid having to fully qualify namespaces here
  mkNumFn←{
    N←#.T.Number
    nonNumber←(N=⊃¨⍵)⍳0
    nonNumber>⍴⍵: N (⍺⍺ (⊃1∘↓)¨⍵)
                  N #.m.E.ty (nonNumber⊃⍵)
  }

  mkRelFn←{
    nonNumber←(#.T.Number=⊃¨⍵)⍳0
    nonNumber>⍴⍵: #.T.bool (⍺⍺ (⊃1∘↓)¨⍵)
    #.T.Number #.m.E.ty (nonNumber⊃⍵)
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
    last←{0=≢2⊃⍵: nil ⋄ ⊃¯1↑2⊃⍵}
    butlast←{0=≢2⊃⍵: nil ⋄ list ¯1↓2⊃⍵}
    append←{(⊃⊃⍵),(,/2∘⊃¨⍵)}
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

  initBaseEnv←{
    e←GLOBAL
    ⎕←#.Printer.print_readably (T.List ({T.String ⍵}¨⊃⍵))
    _←('*ARGV*'      Env.def  (T.List ({T.String ⍵}¨⊃⍵))) e
    _←('envs'        defn     {⎕←#.Env.ENV ⋄ #.T.nil}) e
    _←('nil'         Env.def  nil) e
    _←('apply'       Env.def  T.Builtin 'apply') e
    _←('+'           defOp    (+/)) e
    _←('-'           defOp    (⊃1∘↑-(+/1∘↓))) e
    _←('*'           defOp    (×/))           e
    _←('/'           defOp    (⊃1∘↑÷(×/1∘↓))) e
    _←('>'           defRelOp {∧/ 2>/⍵}) e
    _←('<'           defRelOp {∧/ 2</⍵}) e
    _←('<='          defRelOp {∧/ 2≤/⍵}) e
    _←('>='          defRelOp {∧/ 2≥/⍵}) e
    _←('='           defn     {#.T.bool ⊃∧/ 2 #.m.eq/⍵}) e
    _←('car'         defn     {#.m.lst.car⊃⍵}) e
    _←('cdr'         defn     {#.m.lst.cdr⊃⍵}) e
    _←('last'        defn     {#.m.lst.last⊃⍵}) e
    _←('butlast'     defn     {#.m.lst.butlast⊃⍵}) e
    _←('cons'        defn     {(⊃⍵)#.m.lst.cons 2⊃⍵}) e
    _←('append'      defn     {#.m.lst.append ⍵}) e
    _←('list'        defn     {#.m.lst.list⍵}) e
    _←('list?'       defn     {T.bool #.T.List=⊃⊃⍵}) e
    _←('empty?'      defn     {ty v←⊃⍵ ⋄ T.bool (ty∊#.T.List #.T.Vec)∧(0=≢v)}) e
    _←('str'         defn     {#.T.String (⊃,/#.Printer.print¨⍵)})e
    _←('pr-str'      defn     {#.T.String (¯1↓⊃,/{(#.Printer.print_readably⍵),' '}¨⍵)})e
    _←('prn'         defn     {⎕←(¯1↓⊃,/{(#.Printer.print_readably⍵),' '}¨⍵) ⋄ #.T.nil})e
    _←('println'     defn     {⎕←(¯1↓⊃,/{(#.Printer.print⍵),' '}¨⍵) ⋄ #.T.nil})e
    _←('slurp'       defn     {T.String (⊃⎕nget 2⊃⊃⍵)}) e
    _←('read-string' defn     {#.m.read 2⊃⊃⍵}) e
    _←('eval'        defn     {#.m.GLOBAL#.m.eval⊃⍵}) e
    _←('atom'        defn     (T.newAtom⊃)) e
    _←('atom?'       defn     {T.bool T.Atom≡⊃⊃⍵})e
    _←('deref'       defn     {T.deref⊃⍵}) e
    _←('reset!'      defn     {(⊃⍵) T.set (2⊃⍵)}) e
    _←('count'       defn     {(2-T.Symbol 'nil'≡⊃⍵)⊃(T.Number 0) (T.Number,≢2⊃⊃⍵)})e
    GLOBAL
  }

  evFn←{
    F←lst.car ⍵
    A←2⊃lst.cdr ⍵
    (ty f)←⍺ ⍺⍺ F
    ~ty∊T.Function T.Builtin: (T.Error 'Type error') ⍺
    ⍺ f.call ⍺∘⍺⍺¨A
  }

  evBinding←{
    name form←⍵
    evEnv destEnv←⍺
    val←evEnv ⍺⍺ form
    ((2⊃name) Env.def val) destEnv
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

  evFnStar←{
    env←⍺
    params exp←⍵
    eval←⍺⍺

    F←⎕ns''
    F.params←params
    F.env←env
    F.exp←exp

    T.Function F
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

    T.Symbol 'def!'≡lst.car⍵: ⍺(eval evDef)2⊃lst.cdr⍵

    T.Symbol 'fn*' ≡lst.car⍵: ⍺(eval evFnStar)2⊃lst.cdr⍵

    T.Symbol 'let*'≡lst.car⍵: ⍺{
      (_ bs) exp←1↓2⊃⍵                ⍝ TODO check type!
      bs←({⍺⍵}/(((⍴bs)÷2),2)⍴bs)  ⍝ group by 2
      env←Env.new⍺
      _←(env env∘(eval evBinding))¨SE bs ⍝ Evaluate bindings
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

    FS←lst.car ⍵
    (ty F)←⍺ eval FS
    A←⍺∘eval¨2⊃lst.cdr ⍵

    prepareEnv←{
      F←⍺
      A←⍵

      P←2⊃F.params
      (_ x) y←¯2↑P
      V←1+x≡,'&'                      ⍝ varargs?
      L←lst.list (⊂T.Symbol 'list')∘, ⍝ enclose args in (list ...)
      P←V⊃P ((¯2↓P),⊂y)               ⍝ param names
      A←V⊃A (((¯1+⍴P)↑A),⊂T.List ((¯1+⍴P)↓A)) ⍝ actual args
      bs←{⍺⍵}/(⍪P),(⍪A)               ⍝ list of pairs
      newEnv←Env.new F.env
      _←{((2⊃⊃⍵) Env.def (2⊃⍵)) newEnv}¨SE bs
      newEnv
    }

    F≡'apply': ⍺{
      ty F←⊃A
      A←1↓A
      A←(¯1↓A),2⊃⊃¯1↑A            ⍝ concatenate to last argument

      ~ty∊T.Function T.Builtin: throw 'Type Error: Expected function.'
      ty=T.Builtin: ⍺ F.call A

      ty≠T.Function: (T.Error 'Type error') ⍺

      newEnv←F prepareEnv A
      newEnv eval F.exp
    }⍵

    ⍝ Builtin function call
    ty=T.Builtin: ⍺ F.call A

    ⍝ Type error when non callable
    ty≠T.Function: (T.Error 'Type error') ⍺

    newEnv←F prepareEnv A
    newEnv eval F.exp
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
    loadFile←'(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))'
    swap←'(def! swap! (fn* (a f & args) (reset! a (apply f (deref a) args))))'
    _←GLOBAL eval (read not)
    _←GLOBAL eval (read loadFile)
    _←GLOBAL eval (read swap)
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

  getArgv←{
    argvFile←⎕sh 'echo $ARGV'
    ⎕←argvFile
    0=≢⊃argvFile: ⍬
    S _ _←⎕nget ⊃argvFile
    argv←{(0<≢¨⍵)/⍵} ({(⍵≠⎕ucs 10)/⍵}¨(S=⎕ucs 10)⊂S)

    0=≢argv: argv ⍬

    fname←⊃argv

    fname (1↓argv)
  }

  ∇mapl
   ⎕←'MA(P)L 0.1  =^.^='
   fname ARGV←getArgv⍬
   _←initBaseEnv⊂ARGV
   'ARGV:' ARGV
   init⍬
   :If 0<≢fname
     r←¯1
     code←'(load-file "',fname,'"))'
     _←GLOBAL rep code
   :Else
     r←repIO⍣≡1
     'Bye.'
   :EndIf
   :If r<0
     ⎕off
   :EndIf
  ∇
:EndNamespace
