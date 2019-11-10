:Require file://Types.dyalog
:Require file://Printer.dyalog
:Namespace Errors
  T P←#.(Types Printer)
  L S Str←T.(List Symbol String)

  pr←P.print_readably

  TypeError NameError IndexError UserError←⍳4

  errorForm←{L ((⊂S ⍺),⍵)}

  nameError←{Str ('''',(pr ⍵),''' not found')}
  indexError←{Str ('index ',(pr ⍵),' out of bounds')}
  typeError←{
    L,⊂(S'type-error') (S':expected') (L ({S (P.typeName ⍵)}¨⍺)) (S':found') (S (P.typeName ⊃⍵))
  }
  ty←{
    TypeError throw ⍺ ⍵
  }

  throw←{
    ⍺←0
    p←P.print_readably
    msg←⍺{
      ⍺≡TypeError:  p (⊃⍵)typeError 2⊃⍵
      ⍺≡NameError:  p nameError⍵
      ⍺≡IndexError: p indexError⍵
      p ⍵
    }⍵
    msg ⎕signal 100
  }

:EndNamespace
