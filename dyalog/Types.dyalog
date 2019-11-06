 :Namespace T
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

   ATOMS←⍬

   newAtom←{
     #.T.ATOMS,←⊂⍵
     #.T.Atom (≢#.T.ATOMS)
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
     ⍵≡Error: 'Error'
     ⍵≡Function: 'Function'
     ⍵≡Builtin: 'Builtin'
     ⍵≡List: 'List'
     ⍵≡Map: 'Map'
     ⍵≡Number: 'Number'
     ⍵≡Special: 'Invalid'
     ⍵≡String: 'String'
     ⍵≡Symbol: 'Symbol'
     ⍵≡Bool: 'Bool'
     ⍵≡Vec: 'Vec'
     ⍵≡Atom: 'Atom'
     'Unknown'
   }

 :EndNamespace
