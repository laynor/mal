:Require file://Types.dyalog
:Require file://Printer.dyalog
:Namespace Errors
  T P←#.(Types Printer)
  L S←T.(List Symbol)

  TypeError NameError IndexError UserError←⍳4

  errorForm←{L ((⊂S ⍺),⍵)}

  nameError←'name-error'∘errorForm
  indexError←'index-error'∘errorForm
  typeError←{
    L,⊂(S'type-error') (S':expected') (L ({S (P.typeName ⍵)}¨⍺)) (S':found') (S (P.typeName ⊃⍵))
  }
  ty←{
    TypeError throw ⍺ ⍵
  }

  throw←{
    p←P.print_readably
    msg←⍺{
      ⍺≡TypeError:  p (⊃⍵)typeError 2⊃⍵
      ⍺≡NameError:  p nameError⍵
      ⍺≡IndexError: p indexError⍵
      ⍵
    }⍵
    msg ⎕signal 100
  }

:EndNamespace
