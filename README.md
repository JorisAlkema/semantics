# Projects for the Computational Models and Semantics Course

## Installing Stack

For ease of use, the Haskell projects are based on Stack.
Thus, the first step is to
[install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
on your machine.
After that, you can proceed to the actual project folder and run `stack setup` there
to set up the stack environment and the use `stack build` to build the corresponding project.
The first run may take a while and requires an internet connection because Stack will download
and install GHC and the Haskell required packages.
Further documentation of Stack can be found [here](https://docs.haskellstack.org/en/stable/README/).

## Learning Haskell

Here are some resources for learning Haskell:

* [School of Haskell](https://www.schoolofhaskell.com/) by [FP Complete](https://www.fpcomplete.com)
* Graham Huttons [Programming in Haskell](https://catalogue.leidenuniv.nl/permalink/f/1e3kn0k/TN_cdi_askewsholts_vlebooks_9781316876336); You can find the 2016, 2nd edition on Library Genesis until our library has it.
* [Haskell programming from first principles](https://haskellbook.com/); fast-paced but quite useful
* [Real World Haskell](https://book.realworldhaskell.org/) - when you want to do real programming
  with also the tricky things!
* [Learn you a Haskell for great good](http://learnyouahaskell.com/) - nicely illustrated and useful
  in the beginning

And then some resources for programming in Haskel:

* [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)
* [Documentation of Data.Set](https://hackage.haskell.org/package/containers-0.6.3.1/docs/Data-Set.html)
* [Hoogle](https://hoogle.haskell.org/) for looking up types and functions in
  [Hackage](https://hackage.haskell.org/) packages.
* Haskell wiki [FAQ](https://wiki.haskell.org/FAQ)
* [Try Haskell](https://tryhaskell.org/) in a browser with a little tutorial
* [Haskell.org](https://www.haskell.org/) collects various resources

## Parser Generator

Whenever a parser for some syntax is needed, we will be using the parser generator
[BNFC](https://bnfc.digitalgrammars.com/), which is documented
[here](https://bnfc.readthedocs.io/en/latest/index.html).
For the first project, you will not need to use it yourself though.

## Haskell Community

* [StackOverFlow](http://stackoverflow.com/questions/tagged/haskell)
* [Haskell-beginners](http://haskell.org/mailman/listinfo/beginners) mailing list
* [Haskell-cafe](http://haskell.org/mailman/listinfo/haskell-cafe) mailing list
  (beware: high traffic!)
