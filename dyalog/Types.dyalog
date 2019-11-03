 :Namespace T
   Special Builtin Symbol Number String List Vec Map Function Bool Error←⍳11
   true←Bool 1
   false←Bool 0
   nil←Symbol 'nil'

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
     'Unknown'
   }

 :EndNamespace
