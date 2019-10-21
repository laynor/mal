 :Namespace Env
   ∇R←(fn defineFn name) env
    ns←⎕NS''
    ⎕CS ns
    call←fn
    R←(⊂name ##.ns)⍪env
   ∇

   define←{(⊂⍺⍺ ⍵⍵)⍪⍵}

   get←{
     name←⍺
     2⊃⊃((name≡⊃)¨⍵)/⍵
   }

   callFn←{
     f←⍺⍺get⍵⍵
     f.call ⍵
   }
 :EndNamespace
