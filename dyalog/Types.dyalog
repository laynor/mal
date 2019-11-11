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
   Keyword←':'
   Vec←'V'
   Nil←'⍬'

   true←Bool 1
   false←Bool 0
   nil←Nil 'nil'

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
       0=≢⍺: 1
       ∧/eq/(⍪⍺),⍪⍵
     }
     ty1 v1←⍺
     ty2 v2←⍵
     ∧/ty1 ty2∊List Vec: (0,v1) eqLst (0,v2)
     ∧/ty1 ty2=Map:       (,v1[⍋v1;]) eqLst (,v2[⍋v2;])
                          ⍺≡⍵
   }

   emptyMap←Map (0 2⍴⊂(Number 0) )
   mapGet←{2⊃(2⊃⍵)[⊃⍸⍺∘ eq¨ (2⊃⍵)[;1];]}
   dissoc←{Map ((2⊃⍵)[(~⍺∘ eq¨ tb[;1])/⍳⊃⍴tb;]⊣tb←2⊃⍵)}
   assoc←{
     m←2⊃⍺
     m←(((0.5×⍴⍵),2)⍴⍵)⍪m
     Map (m[m[;1]⍳∪m[;1];])
   }
   mapIn←{∨/⍺∘ eq¨ (2⊃⍵)[;1]}
   keys←{(2⊃⍵)[;1]}

   m←emptyMap assoc (Number 1) (String 'a')   (Number 2) (String 'b')
   m←m assoc        (Number 3) (String 'c')   (Number 4) (String 'd')

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
     ns.(isMacro tag data)←0 ⍺ ⍵
     ns.call←⍺⍺
     Builtin ns
   }

   mkFunction←{
     F←⎕ns''
     F.(isMacro env (params exp))←0 ⍺ ⍵
     Function F
   }

 :EndNamespace
