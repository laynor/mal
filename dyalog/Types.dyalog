 :Namespace Types
   ⍝ Types
   Atom←'A'
   Bool←'B'
   Builtin←'⌺'
   Error←'E'
   Function←'∇'
   List←'L'
   Map←'M'
   Number←'N'
   Special←'⋄'
   String←'S'
   Symbol←'⍺'
   Vec←'V'

   true←Bool 1
   false←Bool 0
   nil←Symbol 'nil'

   ⍝ Lists
   empty←List⍬
   cons←{List ((⊂⍺),2⊃⍵)}
   car←{
     ⍵≡nil: nil
     ⊃(2⊃⍵),⊂nil
   }

   cdr←{
     nil≡⍵: empty
     List (⍬,1↓2⊃⍵)
   }

   nth←{(⊂⍺)⌷2⊃⍵}
   last←{0=≢2⊃⍵: nil ⋄ ⊃¯1↑2⊃⍵}
   butlast←{0=≢2⊃⍵: nil ⋄ (⊃⍵) (¯1↓2⊃⍵)}

   eq←{
     eqLst←{
       (≢⍺)≠≢⍵: 0
       ∧/eq/(⍪⍺),⍪⍵
     }
     ty1 v1←⍺
     ty2 v2←⍵
     ∧/ty1 ty2∊List Vec: (0,v1) eqLst (0,v2)
     ∧/ty1 ty2=Map:       v1 eqLst v2
                          ⍺≡⍵
   }

   ATOMS←⍬

   newAtom←{
     ATOMS,←⊂⍵
     Atom (≢ATOMS)
   }

   deref←{(2⊃⍵)⊃ATOMS}

   set←{
     ATOMS[2⊃⍺]←⊂⍵
     ⍵
   }

   bool←{(1+⍵)⊃false true}


   mkBuiltin←{
     ⍺←0
     ns←⎕NS''
     ns.(tag data)←⍺ ⍵
     ns.call←⍺⍺
     Builtin ns
   }

   mkFunction←{
     F←⎕ns''
     F.(isMacro env (params exp))←0 ⍺ ⍵
     Function F
   }

 :EndNamespace
