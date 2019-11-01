 :Namespace T
   Special Symbol Number String List Vec Map Function Error←⍳9

   typeName←{
     ⍵≡Error: 'Error'
     ⍵≡Function: 'Function'
     ⍵≡List: 'List'
     ⍵≡Map: 'Map'
     ⍵≡Number: 'Number'
     ⍵≡Special: 'Invalid'
     ⍵≡String: 'String'
     ⍵≡Symbol: 'Symbol'
     ⍵≡Vec: 'Vec'
     'Unknown'
   }
 :EndNamespace
