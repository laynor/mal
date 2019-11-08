 :Namespace Types
   ⍝ Special Builtin Symbol Number String List Vec Map Function Bool Atom Error←⍳12
   Special←'⋄'
   Builtin←'⌺'
   Symbol←'⍺'
   Number←'N'
   String←'S'
   List←'L'
   Vec←'V'
   Map←'M'
   Function←'∇'
   Bool←'B'
   Atom←'A'
   Error←'E'

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


   mkFn←{
     ⍺←0
     ns←⎕NS''
     ns.tag←⍺
     ns.call←⍺⍺
     ns.data←⍵
     Builtin ns
   }

   typeName←{
     ⍵≡Error:    'Error'
     ⍵≡Function: 'Function'
     ⍵≡Builtin:  'Builtin'
     ⍵≡List:     'List'
     ⍵≡Map:      'Map'
     ⍵≡Number:   'Number'
     ⍵≡Special:  'Invalid'
     ⍵≡String:   'String'
     ⍵≡Symbol:   'Symbol'
     ⍵≡Bool:     'Bool'
     ⍵≡Vec:      'Vec'
     ⍵≡Atom:     'Atom'
                 'Unknown'
   }

 :EndNamespace
