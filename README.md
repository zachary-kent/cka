# Refinement of Series-Rational Expressions

This is an implementation of the decision procedure detailed [here](https://discovery.ucl.ac.uk/id/eprint/10053782/1/Brunet_On%20decidability%20of%20concurrent%20kleene%20algebra_VoR.pdf) for refinement of series-rational (SR) expressions under the Concurrent Kleene Algebra (CKA) axioms.

## Installation

You'll need OCaml and `opam` to build from source. If you don't have either of these installed, first follow the instructions [here](https://ocaml.org/docs/installing-ocaml) to do so. Then, clone this repo and navigate to its root. Run the following to create a new switch called `cka`, install dependencies, and build the project.


```
opam switch create cka ocaml-base-compiler.5.0.0
opam switch cka
opam install . --deps-only
opam exec -- dune build
```

And that's it! You're now ready to run the REPL and/or test suite.

## REPL

You can check the refinement of two sr-expressions through a simple REPL. After installation, run the following in the root of this repo to start the REPL:

```
opam exec -- dune exec cka
```

It will prompt you to enter two newline-separated sr-expressions and will then tell you whether the first term you entered refines the second. Beware, because this problem is EXPSPACE-complete, the performance is very unpredictable, and your terminal may appear to hang due to the construction of an NFA with more states than atoms in the observable universe. Enjoy!

## Testing

To allay any fears about the correctness of this implementation, you can run `dune test` in the root of this repo the execute the unit test suite, which currently includes tests verifying different instantiations of the CKA axioms.