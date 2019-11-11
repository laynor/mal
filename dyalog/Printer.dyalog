:Require file://Chars.dyalog
:Require file://Types.dyalog
:Namespace Printer
  C T←#.(Chars Types)

  trim←{a←⍵=' ' ⋄ b←~(¯1↓(a,0)∧(1,a))∨(⌽∧\⌽a) ⋄ b/⍵}
  unescape←{
    unescape1←{
      escapes←(('\'∘,)¨'trbn\"'),⍵
      trans←C.(TAB CR BS LF),'\"',⍵
      1↑(⍵=trans)/escapes
    }
    ∊unescape1¨⍵
  }

  typeName←{
    ⍵≡T.Error:    'Error'
    ⍵≡T.Function: 'Function'
    ⍵≡T.Builtin:  'Builtin'
    ⍵≡T.List:     'List'
    ⍵≡T.Map:      'Map'
    ⍵≡T.Number:   'Number'
    ⍵≡T.Special:  'Invalid'
    ⍵≡T.String:   'String'
    ⍵≡T.Symbol:   'Symbol'
    ⍵≡T.Keyword:  'Keyword'
    ⍵≡T.Bool:     'Bool'
    ⍵≡T.Vec:      'Vec'
    ⍵≡T.Nil:      'Nil'
    ⍵≡T.Atom:     'Atom'
                  'Unknown'
  }


  print_readably←{
    t v←2↑⍵
    t≡T.Number:   ('¯'⎕r'-')trim⍕v
    t≡T.Symbol:   v
    t≡T.Keyword:  v
    t≡T.String:   '"',(unescape v),'"'
    t≡T.List:     '(',(trim⍕print_readably¨ v),')'
    t≡T.Vec:      '[',(trim⍕print_readably¨ v),']'
    t≡T.Map:      '{',(trim⍕print_readably¨,v),'}'
    t≡T.Builtin:  '#<Builtin ',(⍕v),'>'
    t≡T.Function: '#<', ('Funciton' 'Macro'⊃⍨1+v.isMacro),' ',(⍕v),'>'
    t≡T.Atom:     '(atom ',(print_readably T.deref ⍵),')'
    ⍵≡T.true:     'true'
    ⍵≡T.false:    'false'
    ⍵≡T.nil:      'nil'
    t≡T.Error:    'ERROR: ', v
    '<',(⍕⍵),'>'                    ⍝ do something better than just returning a string 'error'
  }

  print←{
    t v←2↑⍵
    t≡T.String:  v
    t≡T.Keyword: v
    t≡T.List:    '(',(trim⍕print¨ v),')'
    t≡T.Vec:     '[',(trim⍕print¨ v),']'
    t≡T.Map:     '{',(trim⍕print¨,v),'}'
    print_readably ⍵
  }

:EndNamespace