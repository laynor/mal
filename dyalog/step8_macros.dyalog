:Require file://debug.dyalog
:Require file://display.dyalog

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

  L V M S Str←T.(List Vec Map Symbol String)

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
  nameError←E.(NameError∘throw)

  def←{(⍺⍺ Env.def ⍵⍵)⍵}
  defn←{(⍺⍺ Env.defn ⎕this.⍵⍵) ⍵}

  GLOBAL←1                      ⍝ Global environment

  initBaseEnv←{
    ARGV←L ({Str ⍵}¨⊃⍵)

    B ←1 2⍴ '*ARGV*'                ARGV
    B⍪←{GLOBAL eval⊃⍵}    Env.fbind 'eval'
    B⍪←{⎕←Env.ENV ⋄ nil}  Env.fbind 'envs'
    B⍪←{nil⊣⎕←#.display⍵} Env.fbind 'display'
    _←GLOBAL Env.defAll core.EXPORTS
    _←GLOBAL Env.defAll B

    ⍝ _←('*ARGV*' Env.def ARGV) GLOBAL
    ⍝ _←GLOBAL Env.defAll B
    GLOBAL
  }

  evBinding←{
    name form←⍵
    evEnv destEnv←⍺
    val←evEnv ⍺⍺ form
    ((2⊃name) Env.def val) destEnv
  }

  split←((⊂car),(⊂2⊃cdr))

  eval←{
    isCons←{((⊃⍵)∊L V)∧0<≢2⊃⍵}
    envget←{⍺Env.get 2⊃⍵}

    macroexpand←{
      quote←{core.list (S 'quote') ⍵}
      isMC←{
        S≢⊃car⍵: 0 nil
        t v←⍺Env.get 2⊃car⍵
        t≠T.Function: 0 nil
        v.isMacro (t v)
      }
      ~isCons⍵: ⍵
      res fn←⍺isMC⍵
      ~res: ⍵
      newForm←fn cons (core.list quote¨(2⊃cdr⍵)) ⍝ TODO: extract map
      ⍺eval newForm
    }

    form←⍺macroexpand⍵

    S≡⊃form: ⍺{
      ':'=⊃2⊃form: form         ⍝ keywords
      (2⊃form) Env.in ⍺: ⍺envget form
      nameError 2⊃form
    }⍬

    V M∊⍨⊃form: (⊃form) (⍺∘eval¨SE 2⊃form)  ⍝ Vectors

    L≠⊃form: form               ⍝ Self evaluating stuff
    0=≢2⊃form: form             ⍝ empty lists

    ⍝ Lists

    head tail←split form

    S 'def!'≡head: ⍺{
      name form←tail
      val←⍺ eval form
      _←(((2⊃name) Env.def val) ⍺)
      val
    }⍵

    S 'defmacro!'≡head: ⍺{
      name mFn←tail
      t v←val←⍺ eval mFn
      t≢T.Function: T.Function E.ty val
      v.isMacro←1
      _←(((2⊃name) Env.def val) ⍺)
      val
    }⍬

    S 'fn*' ≡head: ⍺T.mkFunction tail

    S 'let*'≡head: ⍺{
      (_ bs) exp←tail                ⍝ TODO check type!
      bs←({⍺⍵}/(((⍴bs)÷2),2)⍴bs)     ⍝ group by 2
      env←Env.new⍺
      _←(env env∘(eval evBinding))¨SE bs ⍝ Evaluate bindings
      env eval exp
    }⍬

    S 'do'≡head: ⍺{
      x←⍺∘eval¨SE tail
      0=≢x: nil
      ⊃¯1↑x
    }⍬

    S 'if'≡head: ⍺{
      cond then else←3↑tail,⊂nil
      ~(⊂⍺eval cond)∊nil T.false: ⍺eval then
                                  ⍺eval else
    }⍵

    S 'quote'≡head: ⊃tail

    S 'quasiquote'≡head: ⍺{
      qq←{
        ~isCons ⍵:                    L ((S 'quote') ⍵)
        S 'unquote'≡car⍵:             car cdr⍵
        ~isCons car⍵:                 L ((S 'cons')   (∇ car⍵)       (∇cdr⍵))
        ~S 'splice-unquote'≡car car⍵: L ((S 'cons')   (∇ car⍵)       (∇cdr⍵))
                                      L ((S 'concat') (car cdr car⍵) (∇cdr⍵))
      }
      ⍺eval qq⊃tail
    }⍵

    S 'macroexpand-internal'≡head: ⍺macroexpand⍣≡⊃tail

    prepareEnv←{
      F A←⍺ ⍵

      P←2⊃F.params
      (_ x) y←¯2↑P
      V←1+x≡,'&'                              ⍝ varargs?
      P←V⊃P ((¯2↓P),⊂y)                       ⍝ param names
      A←V⊃A (((¯1+⍴P)↑⍬,A),⊂L ((¯1+⍴P)↓⍬,A))  ⍝ actual args
      bs←{⍺⍵}/(⍪P),(⍪A)                       ⍝ list of pairs
      newEnv←Env.new F.env
      _←{((2⊃⊃⍵) Env.def (2⊃⍵)) newEnv}¨SE bs
      newEnv
    }

    ⍝ concatenazione di funzioni applicato a operatore per creare vettore di namespace

    S 'apply-internal'≡head: ⍺{
      A (ty F)←(1∘↑,(⊂1∘↓))tail
      A←(¯1↓A),2⊃⊃¯1↑A          ⍝ concatenate to last argument

      ~ty∊T.Function T.Builtin: T.Function T.Builtin E.ty ty F

      ty=T.Builtin: ⍺ F.call A

      newEnv←F prepareEnv A
      newEnv eval F.exp
    }⍵


    (ty F)←⍺ eval head
    A←⍺∘eval¨SE tail

    ⍝ Builtin function call
    ty=T.Builtin: ⍺ F.call A

    ⍝ Type error when non callable
    ty≠T.Function: T.Function T.Builtin E.ty ty F

    newEnv←F prepareEnv A
    newEnv eval F.exp
  }

  rep←{
    100:: ⎕dmx.EM
    P.print_readably ⍺ eval read ⍵
  }

  init←{
    loadFile←'(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))'
    _←GLOBAL eval read loadFile
    _←GLOBAL eval read '(load-file "core.mal")'
    ⍬
  }

  repIO←{
    1004:: ¯2
    ⍵≤0: ⍵

    ⍞←p←'user> '
    inp←(≢p)↓⍞
    inp≡'': 0
    inp≡'(exit)': ¯1
    ⍞←(GLOBAL rep inp),C.LF
    1+(2|⍵)
  }

  getArgv←{
    argvFile←⎕sh 'echo $ARGV'
    0=≢⊃argvFile: ⍬
    txt←⊃⎕nget ⊃argvFile

    (txt≠C.LF)⊆txt
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
