:Require file://debug.dyalog

:Require file://Chars.dyalog
:Require file://Env.dyalog
:Require file://Errors.dyalog
:Require file://Types.dyalog
:Require file://Reader.dyalog
:Require file://Printer.dyalog

:Require file://core.dyalog

:Namespace m
  ⍝ import
  SE←{0=≢⍵: ⍵ ⋄ ⍺⍺ ⍵}           ⍝ safe each: do not execute when empty vector
  C core Env E P R T←#.(Chars core Env Errors Printer Reader Types)

  ARGV←⍬

  ⍝ importing some names
  car←T.car
  cdr←T.cdr
  nth←T.nth
  cons←T.cons
  nil←T.nil
  read←R.read
  print←P.print_readably

  ⍝ Some utility functions
  typeError←E.(TypeError∘throw)

  def←{(⍺⍺ Env.def ⍵⍵)⍵}
  defn←{(⍺⍺ Env.defn ⎕this.⍵⍵) ⍵}

  GLOBAL←1                      ⍝ Global environment

  initBaseEnv←{
    e←GLOBAL
    ARGV←T.List ({T.String ⍵}¨⊃⍵)

    _←('*ARGV*'      def   ARGV) e
    _←('envs'        defn  {⎕←Env.ENV ⋄ T.nil})        e
    _←('eval'        defn  {GLOBAL eval⊃⍵})            e
    _←('+'           defn  core.plus)                  e
    _←('-'           defn  core.minus)                 e
    _←('*'           defn  core.multiply)              e
    _←('/'           defn  core.divide)                e
    _←('<'           defn  core.lt)                    e
    _←('<='          defn  core.lte)                   e
    _←('='           defn  core.eq)                    e
    _←('>='          defn  core.gte)                   e
    _←('>'           defn  core.gt)                    e
    _←('apply'       def   core.apply)                 e
    _←('atom'        defn  core.atom)                  e
    _←('atom?'       defn  core.isAtom)                e
    _←('butlast'     defn  core.butlast)               e
    _←('car'         defn  core.first)                 e
    _←('cdr'         defn  core.rest)                  e
    _←('concat'      defn  core.concat)                e
    _←('cons'        defn  core.cons)                  e
    _←('count'       defn  core.count)                 e
    _←('deref'       defn  core.deref)                 e
    _←('empty?'      defn  core.isEmpty)               e
    _←('first'       defn  core.first)                 e
    _←('last'        defn  core.last)                  e
    _←('list'        defn  core.list)                  e
    _←('list?'       defn  core.isList)                e
    _←('macroexpand' def   core.macroexpand)           e
    _←('nil'         def   core.nil)                   e
    _←('nth'         defn  core.nth)                   e
    _←('pr-str'      defn  core.prStr)                 e
    _←('println'     defn  core.println)               e
    _←('prn'         defn  core.prn)                   e
    _←('read-string' defn  core.readString)            e
    _←('reset!'      defn  core.reset)                 e
    _←('rest'        defn  core.rest)                  e
    _←('slurp'       defn  core.slurp)                 e
    _←('str'         defn  core.str)                   e
    GLOBAL
  }

  evFn←{
    F←car ⍵
    A←2⊃cdr ⍵
    (ty f)←⍺ ⍺⍺ F
    ~ty∊T.(Function Builtin): typeError T.(Function Builtin) F
    ⍺ f.call ⍺∘⍺⍺¨A
  }

  evBinding←{
    name form←⍵
    evEnv destEnv←⍺
    val←evEnv ⍺⍺ form
    ((2⊃name) Env.def val) destEnv
  }


  ⍝ TODO check name is actually a symbol
  evDef←{
    name form←⍵
    val←⍺ ⍺⍺ form
    _←(((2⊃name) Env.def val) ⍺)
    val
  }


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
  eval←{

    isCons←{((⊃⍵)∊T.(List Vec))∧0<≢2⊃⍵}

    macroexpand←{
      quote←{core.list (T.Symbol 'quote') ⍵}
      isMC←{
        T.Symbol≢⊃car⍵: 0 T.nil
        t v←⍺Env.get 2⊃car⍵
        (t≠T.Function): 0 T.nil
        v.isMacro (t v)
      }
      ~isCons⍵: ⍵
      res fn←⍺isMC⍵
      ~res: ⍵
      newForm←fn cons (core.list quote¨(2⊃cdr⍵))
      ⍺eval newForm
    }

    form←⍺macroexpand⍵

    ty←⊃form

    ty≡T.Symbol: ⍺{
      ':'=⊃2⊃form: form             ⍝ keywords
      (2⊃form) Env.in ⍺: (⍺Env.get(2⊃form))
      E.NameError E.throw 2⊃form
    }⍬

    (~ty∊T.(List Vec Map)): form   ⍝ Other self evaluating stuff

    ty≡T.Vec: T.Vec (⍺∘eval¨SE 2⊃form) ⍝ Vectors

    ty≡T.Map: T.Map (⍺∘eval¨SE 2⊃form) ⍝ Maps

    ⍝ Lists

    0=≢2⊃form: form
    head←car form
    tail←cdr form
    T.Symbol 'def!'≡head:      ⍺(eval evDef)2⊃tail
    T.Symbol 'defmacro!'≡head: ⍺{
      name mFn←2⊃tail
      t v←val←⍺ eval mFn
      t≢T.Function: T.Function E.ty val
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
      car tail
    }⍵

    T.Symbol 'quasiquote'≡head: ⍺{
      qq←{
        L S V←T.(List Symbol Vec)
        ~isCons ⍵:                    L ((S 'quote') ⍵)
        S 'unquote'≡car⍵:             car cdr⍵
        ~isCons car⍵:                 L ((S 'cons')   (∇ car⍵)       (∇cdr⍵))
        ~S 'splice-unquote'≡car car⍵: L ((S 'cons')   (∇ car⍵)       (∇cdr⍵))
                                      L ((S 'concat') (car cdr car⍵) (∇cdr⍵))
      }
      x←qq car tail
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
      ⍺macroexpand⍣≡car tail
    }⍵

    F≡'apply': ⍺{
      ty F←⊃A
      A←1↓A
      A←(¯1↓A),2⊃⊃¯1↑A          ⍝ concatenate to last argument

      ~ty∊T.Function T.Builtin: T.Function T.Builtin E.ty ty F

      ty=T.Builtin: ⍺ F.call A

      newEnv←F prepareEnv A
      newEnv eval F.exp
    }⍵

    ⍝ Builtin function call
    ty=T.Builtin: ⍺ F.call A

    ⍝ Type error when non callable
    ty≠T.Function: T.Function T.Builtin E.ty ty F

    newEnv←F prepareEnv A
    newEnv eval F.exp
  }

  ∇R←env rep input
   :Trap 100
     v←env eval read input
     R←P.print_readably v
   :Case 100
     R←⎕dmx.EM
   :EndTrap
  ∇

  init←{
    loadFile←'(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))'
    _←GLOBAL eval read loadFile
    _←GLOBAL eval read '(load-file "core.mal")'
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
