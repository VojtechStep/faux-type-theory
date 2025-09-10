  $ fauxtt hole.ftt
  A is assumed.
  a is assumed.
  (λ (x : A) ⇒ x) a
       : A
  λ (B : Type) ⇒ λ (b : B) ⇒
    (λ (x : (λ (B1 : Type) ⇒ λ (b1 : B1) ⇒ B1) B b) ⇒ x) b
       : Π (B : Type), Π (b : B), (λ (B1 : Type) ⇒ λ (b1 : B1) ⇒ B1) B
           b
  let B := A in
  (λ (b : B) ⇒
     (λ (x : (let B1 := A in (λ (A : Type) ⇒ λ (b1 : A) ⇒ B1)) B b) ⇒
        x) b)
       : Π (b : A), (let B := A in (λ (A : Type) ⇒ λ (b1 : A) ⇒ B)) A b
  λ (B : Type) ⇒ λ (f : Π (_1 : B), B) ⇒ λ
  (x : (λ (B1 : Type) ⇒ λ (f1 : Π (_1 : B1), B1) ⇒ B1) B f) ⇒ f x
       : Π (B : Type), Π (f : Π (_1 : B), B),
           Π (x : (λ (B1 : Type) ⇒ λ (f1 : Π (_1 : B1), B1) ⇒ B1) B f),
             B
  $ fauxtt unscoped.ftt
  Typechecking error at file "unscoped.ftt", line 1, charaters 44-45:
  this expression should have type B but has type ?X
  $ fauxtt funhole.ftt
  A is assumed.
  a is assumed.
  i is assumed.
  Typechecking error at file "funhole.ftt", line 5, charaters 21-26:
  this expression should be a function but has type ?X
