# Faux Type Theory

These are the materials for the lecture series [Programming language techniques for proof assistants](https://europroofnet.github.io/LFPSI25-Andrej/), delivered by [Andrej Bauer](https://www.andrej.com/en/) at the
[International School on Logical Frameworks and Proof Systems Interoperability (LFPSI)](https://europroofnet.github.io/LFPSI25/),
a [Final EuroProofNet Symposium](https://europroofnet.github.io/Symposium/) event that took place at [Institut Pascal](https://www.institut-pascal.universite-paris-saclay.fr/) on September 8–12, 2025.

The lectures are going to be recorded. A link to the videos will be provided here.

## Lecture 1: From declarative to algorithmic type theory

We study **Faux type theory**, a small type theory with a universe containing itself, dependent products, and local
definitions. We present the theory in traditional declarative style. We then reformulate it to obtain an algorithmic
presentation suitable for implementation.

Material:

* **[Slides with speaker notes](./slides/PL-for-PA-lecture-1-handout.pdf)**

## Lecture 2: A monadic type checker

We implement Faux type theory in OCaml. We use external libraries for parsing and management of bound variables.
The core type checker uses *monadic-style* implementaion that encapsulates the context in a reader monad.

Material:

* **[Slides with speaker notes](./slides/PL-for-PA-lecture-2-handout.pdf)**
* **Implementation:** [`monadic-fauxtt`](./monadic-fauxtt)

## Lecture 3: Holes and unification

## Lecture 4: Variables as computational effects
