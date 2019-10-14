 ∇R←(fn defineFn env) name
  ns←name ⎕NS''
  ⎕CS ns
  call←fn
  R←(name ##.ns)⍪env