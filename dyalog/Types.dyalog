 :Namespace T
   ⍝ Special Builtin Symbol Number String List Vec Map Function Bool Atom Error←⍳12
   Type     ← '∊'
   Special  ← '⋄'
   Builtin  ← '⌺'
   Symbol   ← '⍺'
   Number   ← 'N'
   String   ← 'S'
   List     ← 'L'
   Vec      ← 'V'
   Map      ← 'M'
   Function ← '∇'
   Bool     ← 'B'
   Atom     ← 'A'
   Error    ← 'E'

   ⍝ self evaluating symbols
   true←Bool 1
   false←Bool 0
   nil←Symbol 'nil'

   ⍝ Atom support
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

 :EndNamespace
