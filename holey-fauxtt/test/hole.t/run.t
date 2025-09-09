  $ fauxtt hole.ftt
  A is assumed.
  P is assumed.
  f is assumed.
  λ (a : A), let X := A in ((λ (x : X), x) a)
       : Π (a : A), A
  let X := A in (λ (a : A), (λ (x : X), x) a)
       : Π (a : A), A
  let g := (λ (a : A), λ (b : A), b) in (λ (a : A), λ (b : A), g)
       : Π (a : A), Π (b : A), Π (_3 : A), Π (_4 : A), A
  λ (a : A), λ (b : A), let g := (λ (a1 : A), λ (b1 : A), b1) in g
       : Π (a : A), Π (b : A), Π (_5 : A), Π (_6 : A), A
  λ (a : A), λ (b : A), let g := (λ (a1 : A), λ (b1 : A), f a1) in g
       : Π (a : A), Π (b : A), Π (x : A), Π (y : A), P x
  let h := (λ (x : A), λ (y : A), x) in (λ (x : A), λ (y : A), h)
       : Π (x : A), Π (y : A), Π (_7 : A), Π (_8 : A), A
  let h := (λ (x : A), λ (y : A), x) in (λ (x : A), λ (y : A), y)
       : Π (x : A), Π (y : A), A
  $ fauxtt nonlinear.ftt
  A is assumed.
  Typechecking error at file "nonlinear.ftt", line 3, charaters 40-63:
  unification failed
  $ fauxtt unscoped.ftt
  λ (B : Type), let A := B in A
       : Π (B : Type), Type
  Typechecking error at file "unscoped.ftt", line 3, charaters 38-57:
  unification failed
