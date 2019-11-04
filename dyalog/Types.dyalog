 :Namespace T
   Special Builtin Symbol Number String List Vec Map Function Bool Atom Error←⍳12
   true←Bool 1
   false←Bool 0
   nil←Symbol 'nil'

   ATOMS←⍬

   newAtom←{
     #.T.ATOMS,←⊂⍵
     #.T.Atom (≢#.T.ATOMS)
   }

   deref←{
     (2⊃⍵)⊃ATOMS
   }

   set←{
     ATOMS[2⊃⍺]←⊂⍵
     ⍵
   }

   bool←{
     ⍵: true
     false
   }

   mkFn←{
     ns←⎕NS''
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
