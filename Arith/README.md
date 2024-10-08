# Arith - A simple language for arithmetic expressions

This first project implements a very simple language for arithmetic expressions.
Such expressions are generated by the following grammar

    e ::= n | x | e + e | e * e | e / e | (e)

where n is a floating point number (always has to contain the decimal point!) and x a variable.
Note that this syntax is slightly different compared to the lecture, as it is using the usual
ASCII notation for the arithmetical operations.
Moreover, this version of Arith has variables, which will be given their values via an evaluation
environment, see the [implementation section](#implementation) below.
If you want to know the precise precedences and parsing rules, have a look at
`src/Syntax.bnf` or `src/Syntax/Doc.txt`.

## Building and running

You can build the project by running `stack build` in the folder and then run the program
with `stack run -- [program options]`.
Note that the programs options are preceded by `--`, which cannot be left out!
The allowed program options can be displayed with `stack run -- --help`.
Tests can be run with `stack test`.

## Goals of this project

The goal is to implement various semantics of arithmetic expressions.
You don't have to implement a parser etc., as it is already available.
In the folder `test`, you may find some sample programs to try it out.
For instance, running

    stack run -- test/mixed-ops.simp

results in the output

    $stack run -- test/mixed-ops.simp
    test/mixed-ops.simp

    Parse Successful!

    [Abstract Syntax]

    EPlus (ETimes (EDiv (ETimes (ENum 5.0) (ENum 2.0)) (ENum 2.0)) (ENum 4.0)) (ENum 3.0)

    [Linearized tree]

    5.0 * 2.0 / 2.0 * 4.0 + 3.0

This says that the file `test/mixed-ops.simp` has been read and successfully parsed.
Afterwards, the abstract syntax tree, which internally represents the program as a Haskell data
type, is displayed.
At the end, also the translation of the program back into the concrete input syntax
(linearised tree) is displayed.
Like this, you can try to parse various programs.
Note that you can also run the program without arguments and enter an expression through the
standard input.

Next, you may try to run

    stack run -- -e d "" test/mixed-ops.simp

This command line tries to evaluate (the "-e" flag) the program in `test/mixed-ops.simp` with the
denotational semantics (the "d") in an empty environment (the empty string in the double quotes).
This results in the output

    $ stack run -- -e d "" test/mixed-ops.simp

    Parse Successful!
    test/mixed-ops.simp

    Parse Successful!
    Arith-exe: Undefined case: EPlus (ETimes (EDiv (ETimes (ENum 5.0) (ENum 2.0)) (ENum 2.0)) (ENum 4.0)) (ENum 3.0)
    CallStack (from HasCallStack):
        error, called at src/Semantics.hs:35:19 in Arith-0.1.0.0-<ID>:Semantics

The first successful parse is that of the empty environment and the second of the given program.
After that, it is indicated that the denotational semantics are not defined for some case, this is
line 35 in the file `src/Semantics.hs`.

As you can see in that file, there are various maps that implement the different styles of
semantics.
Your task is it now to fill in these maps with the correct semantics of arithmetic expressions.

## Running in an environment

Arithmetic expressions in Arith may contain variables.
In this case, an environment has to be passed to the semantics.
This can be done by providing the environment as comma-separated list of value assignments like so:

    stack run -- -e d "x = 5.0, y = 2.0" test/open-mixed.simp

The file `src/Semantics.hs` contains a map `getVal` that will help you in looking up values in the
environment.

## Correctness

You can run the unit tests with the command

    stack test

The goal is to implement the semantics and pass all the unit tests.

## Implementation

You will have to implement the three functions `denotation`, `bigStep` and `smallStep` in the
file `src/Semantics.hs`.
Each of these functions has undefined cases that throw an error.
To see what you have to fill in, replace any of the occurrences of `error $ undef e` by an
underscore, a so-called [hole](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/typed_holes.html),
and run `stack build`.
The compiler will tell you now what the type of the expression is that you have to fill in and what
you have in scope.

Common to all semantics is that they take in an environment, in which to look up the value of
variables.
Such an environment is an element of the type `Env`.
You will not need to know much about this type, except that you have to pass the current environment
around and that you can access values of variables in it with `getVal`.
But even that is already implemented for you in all semantics!

### Denotational Semantics

The function `denotation` will implement the denotational semantics of Arith.
This function has type `Env -> Exp -> D`, where `Env` is the data type for environments discussed
above, `Exp` is the type of expressions and `D` is the domain that holds the result of the
denotational semantics.
Recall from the lecture that `D` was given as rational numbers with an extra element that represents
an error.
This is implemented here by applying the
[Maybe](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Maybe)
type constructor to `Double`.
This results in a type with constructors `Nothing :: D` and `Just :: Double -> D`, where `Nothing`
represents a failure and `Just a` represents the successful computation of the number `a`.

In the implementation of `denotation`, you will find that two of the five cases are already given.
Your task is now to fill in the remaining three.
You can consult the documentation of `Maybe` and use existing functions, but you can also work
directly with pattern matching on it.
In the latter case, try to abstract repeating code into reusable functions.

### Big-Step Semantics

The implementation of `bigStep` is fairly straightforward because you can just recursively evaluate
the given expression.
There is only one complication, namely that the big-step relation may be undefined.
This is represented here by having `Set Double` instead of `Double` as codomain of `bigStep`, which
allows you to have an empty set as result when a division by zero occurs.
Think of `bigStep env :: Exp -> Set Double` as representing a relation between expressions and
floating point numbers.

### Small-Step Semantics

The small-step semantics have to implement a relation between pairs `(Env, Exp)` of environments and
expressions and are thus give by a function `(Env, Exp) -> Set (Env, Exp)`, cf. the big-step
semantics.
For your convenience, a function `binaryS` is provided to implement the rules that correspond
to binary operators.
You can consult the case for `ETimes` to understand the usage of `binaryS`.
