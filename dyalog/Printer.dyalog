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


  pprint←{
    t v←⍵
    t≡T.Number: ('¯'⎕r'-')trim⍕v
    t≡T.Symbol: v
    t≡T.String: '"',(unescape v),'"'
    t≡T.List:   '(',(trim⍕pprint¨ v),')'
    t≡T.Vec:    '[',(trim⍕pprint¨ v),']'
    t≡T.Map:    '{',(trim⍕pprint¨ v),'}'
    t≡T.Function: '#<Funciton ',⍕v,'>'
    ⍵≡T.true: 'true'
    ⍵≡T.false: 'false'
    t≡T.Error:  'ERROR: ', v
    'error',⍕⍵                    ⍝ do something better than just returning a string 'error'
  }

:EndNamespace