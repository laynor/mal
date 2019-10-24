:Require file://Types.dyalog
:Namespace Printer
  T←##.T
  trim←{a←⍵=' ' ⋄ b←~(¯1↓(a,0)∧(1,a))∨(⌽∧\⌽a) ⋄ b/⍵}

  pprint←{
    t v←⍵
    t≡T.Number: ('¯'⎕r'-')trim⍕v
    t≡T.Symbol: v
    t≡T.String: '"',(unescape v),'"'
    t≡T.List:   '(',(trim⍕pprint¨ v),')'
    t≡T.Vec:    '[',(trim⍕pprint¨ v),']'
    t≡T.Map:    '{',(trim⍕pprint¨ v),'}'
    t≡T.Function: '#<Funciton>'
    t≡T.Error:  'ERROR: ', v
    'error'                    ⍝ do something better than just returning a string 'error'
  }

:EndNamespace