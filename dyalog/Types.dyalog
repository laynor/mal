 :Namespace T
   Special Symbol Number String List Vec Map Function Bool Error←⍳10
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
     Function ns
   }

   typeName←{
     ⍵≡Error: 'Error'
     ⍵≡Function: 'Function'
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
