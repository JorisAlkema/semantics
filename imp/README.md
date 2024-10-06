# IMP - A simple imperative language

## Syntax

As introduced in the lecture and the book, we use the following grammar.

    a ::= n | x | a + a | a * a | a - a
    b ::= v | a == a | a <= a | b ∧ b | b ∨ b | ¬ b
    c ::= skip | x := a | c ; c | if b then c else c | while b do c

where n is any integer, v `true` or `false` and x any identifier.
Note that the logic connectives are in unicode.
However, also &&, || and ~ are accepted instead of ∧, ∨ and ¬, respectively.

## Building and running

You can build the project by running `stack build` in the folder and then run the program
with `stack run -- [program options]`.
Note that the programs options are preceded by `--`, which cannot be left out!
The allowed program options can be displayed with `stack run -- --help`.
Tests can be run with `stack test`.

## Goals of this project

The goal is to implement big-step and denotational semantics of imperative programs.
You don't have to implement a parser etc., as it is already available.
In the folder `test`, you may find some sample programs to try it out.
For instance, running

    stack exec imp-exe -- test/fibonacci.imp

prints the internal representation of the program as abstract syntax, as better readable syntax
tree and as pretty-printed program.
Like this, you can try to parse various programs.
Note that you can also run the program without arguments and enter an expression through the
standard input and terminating with CTRL-D.

Next, you may try to run

    stack run -- -e b "n=4" test/fibonacci.imp

This command line tries to evaluate (the "-e" flag) the program in `test/fibonacci.imp` with the
big-step semantics (the "b") in with the variable n set to 4.

The first successful parse is that of the empty environment and the second of the given program.
After that, it is indicated that the big-step semantics are not defined for some case in the file
`src/Semantics/BigStep.hs`.

As you can see in that file, there are various maps that implement the different styles of
semantics.
Your task is it now to fill in these maps with the correct semantics of commands etc. in IMP.

## Correctness

You can run the unit tests with the command

    stack test

The goal is to implement the semantics and pass all the unit tests.

## Implementation

You will have to implement the functions `evalCom` in `Semantics.BigStep` and
`evalCom` in `Semantics.Denotational`, and complete the function `evalBExp` in
`Semantics.ExpEval`.
Each of these functions has undefined cases that throw an error.
To see what you have to fill in, replace any of the occurrences of `failure x` by an
underscore, a so-called [hole](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/typed_holes.html),
and run `stack build`.
The compiler will tell you now what the type of the expression is that you have to fill in and what
you have in scope.

Common to all semantics is that they work with memory, which is defined in `Semantics.Memory`.
Most important for you is that it comes with two functions for reading and updating memory:

    getVal :: Ident -> Mem -> Integer
    updateMem :: Ident -> Integer -> Mem -> Mem

`Ident` is the type of identifiers that are used to refer to memory locations in IMP programs.

### Big-Step Semantics

The implementation of `Semantics.BigStep.evalCom` is fairly straightforward because you can just
recursively evaluate the given expression.
Note that this function will have to diverge on non-termination IMP programs.


### Denotational Semantics
Implementing `Semantics.Denotational.evalCom` is somewhat more complex because it will have to keep
track computation steps.
The goal is to follow the development from the lecture and rely on a type of partial functions
that is defined via

    type D a = Mem -> Delay a

in `Semantics.Denotational`.
The type `Delay` is defined in the module `Semantics.Delay` as follows.

    data Delay a = Now a | Later (Delay a)

Think of `Delay a` as the flat poset on `a`, only that this type keeps also track of computation
steps that are needed to obtain a result.
More precisely, the map `pure :: a -> Delay a` embeds the type `a` via `Now`, which you can think of
as an immediate result, while `Later . Now` delays a computation by one step.
The order on `Delay a` is similar to that on the flat poset: `Now x` is only related to `Now y` if
`x == y`, but `Later u` is below `Now x` for any `u` and `x`.
On the next level, `Later (Now x)` and `Later (Now y)` are related only if `x == y` etc.
This results in the following graph representation of the order.


    Now 1          Now 2
      |  \------- /--   \
      |          /   \   \
    Later (Now 1)     Later (Now 2)
                  .
                  .
                  .
                \   /
                 bot

For you, it is important to know that `Delay` implements the `Monad` interface, which allows you
to sequentially run computations in `Delay` by using the bind operator `>>=`.
As a hint, have a look at the operator `>=>` in `Control.Monad`
(https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Monad.html#v:-62--61--62-).
The map `pure` allows you to return the result of basic computations.
Moreover, you need to know that the tests require that every unfolding of a while-loop, when the
condition is true, needs to produce one `Later`-step.
