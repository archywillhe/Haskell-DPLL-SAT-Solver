# This is a simple implementation of the Classical DPLL Algorithm for solving SAT, written in Haskell

It uses a Happy parser for parsing [CNF](https://en.wikipedia.org/wiki/Conjunctive_normal_form) formula in [DIMACS](http://www.satcompetition.org/2009/format-benchmarks2009.html).

To run it simply do

```
cabal install alex
cabal install happy
cabal configure
cabal run < cnf
```