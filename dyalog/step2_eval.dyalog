 :Require file://debug.dyalog
 :Require file://env.dyalog
 :Require file://Types.dyalog
 :Namespace m
   T←##.T
   ⍝ Debug
   ⍝ Parser combinators
   ⍝ ==================
   ⍝ Parsers are functions that take an ⍝ array of tokens and return an array of
   ⍝ 3 values:
   ⍝
   ⍝ s(tatus) |r(esult)| R(est)     ←  parser input
   ⍝----------|--------|-----------|
   ⍝    1     | result | (k↓input) | -→ SUCCESS
   ⍝    0     | empty  | input     | -→ FAILURE

   Ok←1                          ⍝ Results are array of 3 values:
   fail←{0 (0⍴⍵) ⍵}              ⍝ (0 or 1: failure or success)

   eof←(0=≢),(⊂0∘⍴),⊂            ⍝ parses the end of file
   ∆t←{                          ⍝ {⍵∊⎕D} ∆t <--> parses a character if it is a digit
     0=≢⍵: fail ⍵
     ⍺⍺ ⊃⍵: Ok (⊃⍵) (1↓⍵)
     fail ⍵
   }
   only←{(=∘⍺⍺) ∆t ⍵}            ⍝ 'W' only <--> parses only the character 'W'
   map←{s r R←⍵⍵ ⍵ ⋄ s (⍺⍺ r) R}
   pipe←{⍵⍵ map ⍺⍺ ⍵}

   ⍝ sequences
   seq←{                         ⍝ sequence opening
     s1 r1 R1←⍺⍺ ⍵

     s1: r1 map ⍵⍵ R1
     fail ⍵
   }

   sq←{                          ⍝ other elements in the sequence
     s r R←⍺⍺ ⍵

     s: (r,⊂) map ⍵⍵ R
     fail ⍵
   }

   many←{                        ⍝ parse 0 or more occurrences of ⍺⍺
     P←⍺⍺ ⋄ s r R1←⍺⍺ ⍵

     s: {
       s rs R2←P many R1
       Ok  ((⊂r),rs) R2
     }⍬
     Ok r R1
   }

   some←{((⊃,/) map ((⊂ map ⍺⍺) seq (⍺⍺ many))) ⍵} ⍝ 1 or more ⍺⍺
   ⍝ returns the first parser that succeeds
   alt←{s r R←⍺⍺ ⍵ ⋄ s: s r R ⋄ ⍵⍵ ⍵}

   or←{(1+>/(≢3∘⊃)¨R)⊃R←(⍺⍺ ⍵) (⍵⍵ ⍵)} ⍝ returns result of the parser that
                                       ⍝ consumed more tokens (greedy)
   skip←{(⊢∘(0⍴⍵)) map ⍺⍺ ⍵} ⍝ can't use yet, as sequencing operations die as ⍝ a
                             ⍝ result of using this one.

   flat←{∊map ⍺⍺ ⍵} ⍝ Not always the right way to do it - gotta find the right
                    ⍝ definition

   ⍝ READER
   ⍝ ======

   ⍝ Example lexers
   isAlpha←{⍵∊'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'}
   isDigit←{⍵∊'0123456789'}
   isAlphaNum←isAlpha∨isDigit
   DIGIT←isDigit ∆t
   alpha←isAlpha ∆t
   alphaNum←isAlphaNum ∆t


   ⍝ Lexing - basic chars

   ⍝ Special chars
   :Namespace C
     BS←⎕ucs 8
     TAB←⎕ucs 9
     LF←⎕ucs 10
     CR←⎕ucs 13
     NL←⎕ucs 10
     SPC←' '
     WS←TAB,SPC
     WSNL←NL,WS
   :EndNamespace

   NL←∊∘C.NL ∆t
   isWS1←∊∘C.WSNL
   isWS←∧/∘isWS1
   WS1←isWS1 ∆t
   WS←WS1 some flat

   ⍝ MAL TOKENS
   ⍝ ----------

   ⍝ Strings
   escape←{                   ⍝ we append ⍵ (the second char in the
     escapes←'trbn',⍵         ⍝ escape sequence) so that it is
     trans←C.(TAB CR BS LF),⍵ ⍝ translated to itself when it's not a
     1↑(⍵=escapes)/trans          ⍝ known escape sequence
   }

   unescape←{
     unescape1←{
       escapes←(('\'∘,)¨'trbn\"'),⍵
       trans←C.(TAB CR BS LF),'\"',⍵
       1↑(⍵=trans)/escapes
     }
     ∊unescape1¨⍵
   }

   ⍝ STRING : parse a string literal, escaped with the usual C rules.
   ⍝          returns the escaped string
   STRING←{
     REST←{
       c←1↑⍵
       y←1↑1↓⍵

       0=≢⍵:  fail inp
       c='"': Ok '' (1↓⍵)
       c='\': (escape y)∘, map ∇ 2↓⍵
       {c,⍵} map ∇ 1↓⍵
     }

     {⊃1↓⍵} map ('"' only seq REST) ⍵
   }


   ⍝ QSTRING : parse a string literal, return the string literal itself
   QSTRING←{
     REST←{
       c←1↑⍵
       cc←2↑⍵

       0=≢⍵: fail inp
       c='\': ,⍨∘2↑⍵ map ∇ 2↓⍵
       c='"': Ok '"' (1↓⍵)
       {c,⍵} map ∇ 1↓⍵
     }
     {∊⍵} map ('"' only seq REST) ⍵
   }

   SEMI←';'only
   specialChars←'''~@`()[]{}^'
   nonSymbol←specialChars,'";,',C.WSNL
   SYMCHAR_NOT_DIGIT←{~⍵∊nonSymbol,⎕D,'.'}∆t
   SYMCHAR←(~∊∘nonSymbol)∆t
   COMMA←(=∘',')∆t

   COMMENT←(SEMI seq ({~⍵∊C.NL}∆t many) sq NL) flat

   INT←DIGIT some
   SYM←∊map ((DIGIT many) seq SYMCHAR_NOT_DIGIT sq (SYMCHAR many))

   SPECIAL←∊∘specialChars ∆t

   flt←{(⍺⍺¨⍵)/⍵}


   isComma←≡∘','
   isComment←';'≡⊃
   isWSOrComment←isWS∨isComment∨isComma

   toInt←{0<≢⍵: ⍎⍵ ⋄ ⍵}

   ⍝ Tokens represented as a pair TokType value


   tokType←⊃
   tokVal←{1↑1↓⍵}

   tok←       WS or COMMENT or COMMA
   tok←tok or (T.Special map SPECIAL)
   tok←tok or ((T.Number,toInt) map INT)
   tok←tok or (T.Symbol map SYM)
   tok←tok or (T.String map STRING)

   tokens←(~isWSOrComment) flt map (tok many)

   tt←{x←⍺ ⋄ {x=tokType⍵}∆t ⍵}
   String←T.String∘tt
   Num←T.Number∘tt
   Sym←T.Symbol∘tt
   isSpecial←{ty val←⍺ ⋄ (ty=T.Special)∧(val≡⍵)} ⍝ Ex: (Special '~') isSpecial '~' <--> 1
   Spec←{(isSpecial∘⍺⍺)∆t ⍵}

   ⍝ In most of these parsers, ⍺⍺ is mForm. I don't know if there's a better way to
   ⍝ do mutual recursion

   hDelim←{{(⊃1↓⍵)} map ((⊂map(⍺⍺[1] Spec)) sq (⍵⍵ many) sq (⍺⍺[2] Spec)) ⍵}

   List←{T.List map ('()' hDelim ⍺⍺) ⍵}
   Vec←{T.Vec map ('[]' hDelim ⍺⍺) ⍵}
   Map←{T.Map map ('{}' hDelim ⍺⍺) ⍵}

   mkFnAppl←{T.List ((⊂T.Symbol ⍺),⍵)}

   specialHelper←{
     s r R←(((⊃⍵⍵) Spec) seq ⍺⍺) ⍵
     s : Ok ((1↓⍵⍵) mkFnAppl 1↓r) R
     fail ⍵
   }

   Quote←{(⍺⍺ specialHelper '''quote') ⍵}
   Quasiquote←{(⍺⍺ specialHelper '`quasiquote')⍵}
   Deref←{(⍺⍺ specialHelper '@deref')⍵}
   UnquoteOrSpliceUnquote←{
     spu←{'splice-unquote' mkFnAppl 2↓⍵} map (('~' Spec) seq ('@' Spec) sq ⍺⍺)
     unq←{'unquote' mkFnAppl 1↓⍵} map(('~' Spec) seq ⍺⍺)
     spu or unq ⍵
   }

   WithMeta←{{T.List ((⊂T.Symbol 'with-meta'),⌽1↓⍵)} map ('^' Spec seq ⍺⍺ sq ⍺⍺) ⍵}

   Form←{
     p←Num or Sym or String
     p←p or (∇ List)  or (∇ Vec)        or (∇ Map)
     p←p or (∇ Quote) or (∇ Quasiquote) or (∇ UnquoteOrSpliceUnquote)
     p←p or (∇ Deref) or (∇WithMeta)

     p ⍵
   }
   trim←{a←⍵=' ' ⋄ b←~(¯1↓(a,0)∧(1,a))∨(⌽∧\⌽a) ⋄ b/⍵}

   pprint←{
     t v←⍵
     t≡T.Number: trim⍕v
     t≡T.Symbol: v
     t≡T.String: '"',(unescape v),'"'
     t≡T.List:   '(',(trim⍕pprint¨ v),')'
     t≡T.Vec:    '[',(trim⍕pprint¨ v),']'
     t≡T.Map:    '{',(trim⍕pprint¨ v),'}'
     t≡T.Function: '#<Funciton>'
     t≡T.Error:  'ERROR: ', v
     'error'                    ⍝ do something better than just returning a string 'error'
   }

   read←{
     s r R←tokens ⍵
     s: {
       s r R←Form ⍵
       s: r
       T.Error 'end of input'
     }r
     T.Error 'Lexer error'
   }

   mkPureFn←{
     (⍺⍺ ⍵), ⍺
   }

   D←{(⍺⍺ ##.Env.defn ⍵⍵) ⍵}
   P←{(⍺⍺ D (⍵⍵ mkPureFn)) ⍵}

   mkBaseEnv←{
     e←('+' P (+/)) ⍬
     e←('-' P(1∘↑-(+/1∘↓))) e
     e←('*' P (×/) ) e
     e←('/' P (1∘↑÷(×/1∘↓))) e
     e
   }



   BaseEnv←mkBaseEnv⍬

   ⍝ vEach←{
   ⍝   f←⍺⍺
   ⍝   env←⍺
   ⍝   vec←⍵
   ⍝   0=≢⍵: ⍬ env
   ⍝   {
   ⍝     v  env1←env f ⊃⍵
   ⍝     vs env2←env1 (f ∇) 1↓⍵
   ⍝     ((⊂v),vs) env2
   ⍝   }⍵
   ⍝ }

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
     fsym←⊃⍵
     args←1↓⍵
     ev←⍺⍺
     env←⍺
     f env1←⍺ ⍺⍺ fsym
     T.Function≠⊃f: T.Error 'Type error'
     {
       args env2←env1 (ev vEach) ⍵
       env2 f args
     }⍵
   }

   eval←{
     ty←⊃⍵
     ty≡T.Number: ⍵ ⍺
     ty≡T.String: ⍵ ⍺
     ty≡T.Function: ⍵ ⍺
     ty≡T.Error: ⍵ ⍺
     ty≡T.Symbol: ⍺{
       (2⊃⍵) ##.Env.in ⍺: ((2⊃⍵)##.Env.get⍺) ⍺
       (T.Error ('Name `',(2⊃⍵),''' is unbound.')) ⍺
     }⍵
     ty≡T.Vec: ⍺{
       vs env←(⍺(eval vEach)2⊃⍵)
       (T.Vec vs) env
     }⍵
     ty≡T.Map: T.Map (⍺(∇vEach)2⊃⍵)
     ty≡T.List: ⍺(∇evFn)⍵
     T.Error (⍕'Unknown type' ty)
   }
   print←pprint
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
    ⍝ ⎕off
   ∇
 :EndNamespace
