:Require file://Types.dyalog
:Require file://Printer.dyalog
:Namespace Errors
  T P←#.(Types Printer)
  TypeError NameError IndexError UserError←⍳4

  nameError←{'Name error: ''',⍵,''' not found.'}
  indexError←{'Index error, ', P.print ⍵}
  typeError←{('Type Error: expected ', (⊃{⍺,', ',⍵}/P.typeName¨ ⍺), ', found ', (P.typeName ⊃⍵)),'.'}
  ty←{
    TypeError throw ⍺ ⍵
  }

  throw←{
    msg←⍺{
      ⍺≡TypeError: (⊃⍵)typeError 2⊃⍵
      ⍺≡NameError: nameError⍵
      ⍺≡IndexError: indexError⍵
      ⍵
    }⍵
    msg ⎕signal 100
  }

:EndNamespace
