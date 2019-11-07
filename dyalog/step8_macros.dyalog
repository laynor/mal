:Require file://debug.dyalog
:Require file://env.dyalog
:Require file://Types.dyalog
:Require file://Reader.dyalog
:Require file://Printer.dyalog
:Require file://C.dyalog
:Require file://core.dyalog
:Namespace m
  T←#.T
  Env←#.Env
  C←#.C
  ARGV←⍬
  core←#.core

  :Namespace E
    nameError←{'Name error: ''',⍵,''' not found.'}

    indexError←{'Index error, ', #.Printer.print ⍵}

    ty←{
      msg←#.('Type Error: expected ', (⊃{⍺,', ',⍵}/T.typeName¨ ⍺), ', found ', (T.typeName ⊃⍵)),' (', (⍕⍵), ').'
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
    _←('*ARGV*'      Env.def  (T.List ({T.String ⍵}¨⊃⍵))) e
    _←('envs'        defn     {⎕←#.Env.ENV ⋄ #.T.nil}) e
    _←('nil'         Env.def  nil) e
    _←('apply'       Env.def  T.Builtin 'apply') e
    _←('macroexpand' Env.def  T.Builtin 'macroexpand') e
    _←('+'           defOp    (+/)) e
    _←('-'           defOp    (⊃1∘↑-(+/1∘↓))) e
    _←('*'           defOp    (×/))           e
    _←('/'           defOp    (⊃1∘↑÷(×/1∘↓))) e
    _←('>'           defRelOp {∧/ 2>/⍵}) e
    _←('<'           defRelOp {∧/ 2</⍵}) e
    _←('<='          defRelOp {∧/ 2≤/⍵}) e
    _←('>='          defRelOp {∧/ 2≥/⍵}) e
    _←('='           defn     {#.T.bool ⊃∧/ 2 #.m.eq/⍵}) e
    _←('car'         defn     {#.core.car⊃⍵}) e
    _←('first'       defn     {#.core.car⊃⍵}) e
    _←('cdr'         defn     {#.core.cdr⊃⍵}) e
    _←('rest'        defn     {#.core.cdr⊃⍵}) e
    _←('nth'         defn     {⊃i #.core.nth(#.core.concat (⊃⍵) (#.T.List (i⍴⊂#.T.nil)))⊣i←1+2⊃2⊃⍵}) e
    _←('last'        defn     {#.core.last⊃⍵}) e
    _←('butlast'     defn     {#.core.butlast⊃⍵}) e
    _←('cons'        defn     {(⊃⍵)#.core.cons 2⊃⍵}) e
    _←('concat'      defn     {#.core.concat ⍵}) e
    _←('list'        defn     {#.core.list⍵}) e
    _←('list?'       defn     {T.bool #.T.List=⊃⊃⍵}) e
    _←('empty?'      defn     {ty v←⊃⍵ ⋄ T.bool (ty∊#.T.List #.T.Vec)∧(0=≢v)}) e
    _←('str'         defn     {#.T.String (⊃,/#.Printer.print¨⍵)})e
    _←('pr-str'      defn     {#.T.String (¯1↓⊃,/{(#.Printer.print_readably⍵),' '}¨#.m.SE ⍵)})e
    _←('prn'         defn     {⍞←(¯1↓⊃,/{(#.Printer.print_readably⍵),' '}¨⍵),#.C.LF ⋄ #.T.nil})e
    _←('println'     defn     {⍞←(¯1↓⊃,/{(#.Printer.print⍵),' '}¨⍵),#.C.LF ⋄ #.T.nil})e
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
    F←core.car ⍵
    A←2⊃core.cdr ⍵
    (ty f)←⍺ ⍺⍺ F
    ~ty∊T.Function T.Builtin: T.Function T.Builtin #.m.E.ty F
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
    F.isMacro←0

    T.Function F
  }
  display←{⎕IO ⎕ML←0 1                        ⍝ Boxed display of array.

      box←{                                   ⍝ box with type and axes
          vrt hrz←(¯1+⍴⍵)⍴¨'│─'               ⍝ vert. and horiz. lines
          top←'─⊖→'[¯1↑⍺],hrz                 ⍝ upper border with axis
          bot←(⊃⍺),hrz                        ⍝ lower border with type
          rgt←'┐│',vrt,'┘'                    ⍝ right side with corners
          lax←'│⌽↓'[¯1↓1↓⍺],¨⊂vrt             ⍝ left side(s) with axes,
          lft←⍉'┌',(↑lax),'└'                 ⍝ ... and corners
          lft,(top⍪⍵⍪bot),rgt                 ⍝ fully boxed array
      }

      deco←{⍺←type open ⍵ ⋄ ⍺,axes ⍵}         ⍝ type and axes vector
      axes←{(-2⌈⍴⍴⍵)↑1+×⍴⍵}                   ⍝ array axis types
      open←{16::(1⌈⍴⍵)⍴⊂'[ref]' ⋄ (1⌈⍴⍵)⍴⍵}   ⍝ exposure of null axes
      trim←{(~1 1⍷∧⌿⍵=' ')/⍵}                 ⍝ removal of extra blank cols
      type←{{(1=⍴⍵)⊃'+'⍵}∪,char¨⍵}            ⍝ simple array type
      char←{⍬≡⍴⍵:'─' ⋄ (⊃⍵∊'¯',⎕D)⊃'#~'}∘⍕    ⍝ simple scalar type
      line←{(6≠10|⎕DR' '⍵)⊃' -'}              ⍝ underline for atom

      {                                       ⍝ recursive boxing of arrays:
          0=≡⍵:' '⍪(open ⎕FMT ⍵)⍪line ⍵       ⍝ simple scalar
          1 ⍬≡(≡⍵)(⍴⍵):'∇' 0 0 box ⎕FMT ⍵     ⍝ object rep: ⎕OR
          1=≡⍵:(deco ⍵)box open ⎕FMT open ⍵   ⍝ simple array
          ('∊'deco ⍵)box trim ⎕FMT ∇¨open ⍵   ⍝ nested array
      }⍵
  }
  eval←{

    isCons←{((⊃⍵)∊T.(List Vec))∧0<≢2⊃⍵}

    macroexpand←{
      quote←{core.list (T.Symbol 'quote') ⍵}
      isMC←{
        T.Symbol≢⊃core.car⍵: 0 T.nil
        t v←⍺Env.get 2⊃core.car⍵
        (t≠T.Function): 0 T.nil
        v.isMacro (t v)
      }
      ~isCons⍵: ⍵
      res fn←⍺isMC⍵
      ~res: ⍵
      newForm←fn core.cons (core.list quote¨(2⊃core.cdr⍵))
      ast←⍺eval newForm
      ast
    }

    form←⍺macroexpand⍵

    ty←⊃form

    ty≡T.Symbol: ⍺{
      ':'=⊃2⊃form: form             ⍝ keywords
      (2⊃form) Env.in ⍺: (⍺Env.get(2⊃form))
      throw E.nameError 2⊃form
    }⍬

    (~ty∊T.(List Vec Map)): form   ⍝ Other self evaluating stuff

    ty≡T.Vec: T.Vec (⍺∘eval¨SE 2⊃form) ⍝ Vectors

    ty≡T.Map: T.Map (⍺∘eval¨SE 2⊃form) ⍝ Maps

    ⍝ Lists

    0=≢2⊃form: form
    head←core.car form
    tail←core.cdr form
    T.Symbol 'def!'≡head:      ⍺(eval evDef)2⊃tail
    T.Symbol 'defmacro!'≡head: ⍺{
      name mFn←2⊃tail
      t v←val←⍺ eval mFn
      t≢T.Function: T.Function #.m.e.ty val
      v.isMacro←1
      _←(((2⊃name) Env.def val) ⍺)
      val
    }⍬

    T.Symbol 'fn*' ≡head: ⍺(eval evFnStar)2⊃tail

    T.Symbol 'let*'≡head: ⍺{
      (_ bs) exp←2⊃tail                ⍝ TODO check type!
      bs←({⍺⍵}/(((⍴bs)÷2),2)⍴bs)  ⍝ group by 2
      env←Env.new⍺
      _←(env env∘(eval evBinding))¨SE bs ⍝ Evaluate bindings
      env eval exp
    }⍬
    T.Symbol 'do'≡head: ⍺{
      forms←2⊃tail
      x←⍺∘eval¨SE forms
      0=≢x: T.nil
      ⊃¯1↑x
    }⍬

    T.Symbol 'if'≡head: ⍺{
      cond then else←3↑(2⊃tail),⊂T.nil
      c←⍺eval cond
      ~(⊂c)∊nil T.false: ⍺eval then
      ⍺eval else
    }⍵

    T.Symbol 'quote'≡head: ⍺{
      core.car tail
    }⍵

    T.Symbol 'quasiquote'≡head: ⍺{
      qq←{
        L S V←T.(List Symbol Vec)
        car←core.car
        cdr←core.cdr

        ~isCons ⍵:                    L ((S 'quote') ⍵)
        S 'unquote'≡car⍵:             car cdr⍵
        ~isCons car⍵:                 L ((S 'cons')   (∇ car⍵)       (∇cdr⍵))
        ~S 'splice-unquote'≡car car⍵: L ((S 'cons')   (∇ car⍵)       (∇cdr⍵))
                                      L ((S 'concat') (car cdr car⍵) (∇cdr⍵))
      }
      x←qq core.car tail
      ⍺eval x
    }⍵

    (ty F)←⍺ eval head
    A←⍺∘eval¨SE 2⊃tail

    prepareEnv←{
      F A←⍺ ⍵

      P←2⊃F.params
      (_ x) y←¯2↑P
      V←1+x≡,'&'                              ⍝ varargs?
      L←core.list (⊂T.Symbol 'list')∘,        ⍝ enclose args in (list ...)
      P←V⊃P ((¯2↓P),⊂y)                       ⍝ param names
      A←V⊃A (((¯1+⍴P)↑⍬,A),⊂T.List ((¯1+⍴P)↓⍬,A)) ⍝ actual args
      bs←{⍺⍵}/(⍪P),(⍪A)                       ⍝ list of pairs
      newEnv←Env.new F.env
      _←{((2⊃⊃⍵) Env.def (2⊃⍵)) newEnv}¨SE bs
      newEnv
    }

    F≡'macroexpand': ⍺{
      ⍺macroexpand⍣≡core.car tail
    }⍵

    F≡'apply': ⍺{
      ty F←⊃A
      A←1↓A
      A←(¯1↓A),2⊃⊃¯1↑A          ⍝ concatenate to last argument

      ~ty∊T.Function T.Builtin: T.Function T.Builtin #.m.e.ty ty F

      ty=T.Builtin: ⍺ F.call A

      newEnv←F prepareEnv A
      newEnv eval F.exp
    }⍵

    ⍝ Builtin function call
    ty=T.Builtin: ⍺ F.call A

    ⍝ Type error when non callable
    ty≠T.Function: T.Function T.Builtin #.m.E.ty ty F

    newEnv←F prepareEnv A
    newEnv eval F.exp
  }

  print←##.Printer.pprint

  ∇R←env rep input
   :Trap 100
     v←env eval read input
     R←print v
   :Case 100
     ⎕←⎕dmx.EM
     R←(T.Symbol ,⊂'nil') env
   :EndTrap
  ∇

  init←{
    not ←'(def! not (fn* [o] (if o false true)))'
    loadFile←'(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))'
    swap←'(def! swap! (fn* (a f & args) (reset! a (apply f (deref a) args))))'
    cond←'(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list ''if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons ''cond (rest (rest xs)))))))'
    unless2←'(defmacro! unless2 (fn* (cond then else) `(if (not ~cond) ~then ~else)))'
    _←GLOBAL eval (read not)
    _←GLOBAL eval (read loadFile)
    _←GLOBAL eval (read swap)
    _←GLOBAL eval (read cond)
    _←GLOBAL eval (read unless2)
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
         ⍞←res,C.LF
         R←recur+1
       :EndSelect
     :EndIf
   :Else
     R←¯2
   :EndTrap
  ∇

  getArgv←{
    argvFile←⎕sh 'echo $ARGV'
    0=≢⊃argvFile: ⍬
    S _ _←⎕nget ⊃argvFile

    (S≠C.LF)⊆S
  }



  ∇mapl
   Banner ←'⍵⍵⍵⍵⍵⍵⍵⍵⍵⍵⍵⍵⍵⍵⍵⍵⍵⍵⍵',C.LF
   Banner,←'⍵  MA(P)L  =^⍵^=  ⍵',C.LF
   Banner,←'⍵  Version  0.2   ⍵',C.LF
   Banner,←'⍵⍵⍵⍵⍵⍵⍵⍵⍵⍵⍵⍵⍵⍵⍵⍵⍵⍵⍵',C.LF
   ARGV←getArgv⍬
   _←initBaseEnv⊂1↓ARGV
   init⍬
   :If 0<≢ARGV
     r←¯1
     code←'(load-file "',(⊃ARGV),'"))'
     _←GLOBAL rep code
   :Else
     ⍝ Banner suppressed for testing reasons
     ⍝ ⍞←Banner
     r←repIO⍣≡1
     'Bye.'
   :EndIf
   :If r<0
     ⎕off
   :EndIf
  ∇
:EndNamespace
