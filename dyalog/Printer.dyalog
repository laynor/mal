:Require file://Types.dyalog
:Require file://C.dyalog
:Namespace Printer
  C←##.C
  T←##.T
  trim←{a←⍵=' ' ⋄ b←~(¯1↓(a,0)∧(1,a))∨(⌽∧\⌽a) ⋄ b/⍵}
  unescape←{
    unescape1←{
      escapes←(('\'∘,)¨'trbn\"'),⍵
      trans←C.(TAB CR BS LF),'\"',⍵
      1↑(⍵=trans)/escapes
    }
    ∊unescape1¨⍵
  }

  print_readably←{
    t v←⍵
    t≡T.Number: ('¯'⎕r'-')trim⍕v
    t≡T.Symbol: v
    t≡T.String: '"',(unescape v),'"'
    t≡T.List:   '(',(trim⍕print_readably¨ v),')'
    t≡T.Vec:    '[',(trim⍕print_readably¨ v),']'
    t≡T.Map:    '{',(trim⍕print_readably¨ v),'}'
    t≡T.Builtin: '#<Builtin ',(⍕v),'>'
    t≡T.Function: '#<', ('Funciton' 'Macro'⊃⍨1+v.isMacro),' ',(⍕v),'>'
    t≡T.Atom: '(atom ',(print_readably T.deref ⍵),')'
    ⍵≡T.true: 'true'
    ⍵≡T.false: 'false'
    t≡T.Error:  'ERROR: ', v
    '<',(⍕⍵),'>'                    ⍝ do something better than just returning a string 'error'
  }

  print←{
    t v←⍵
    t≡T.String: v
    t≡T.List:   '(',(trim⍕print¨ v),')'
    t≡T.Vec:    '[',(trim⍕print¨ v),']'
    t≡T.Map:    '{',(trim⍕print¨ v),'}'
    print_readably ⍵
  }


  pprint←print

:EndNamespace