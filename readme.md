# This is a simple implementation of the Classical DPLL Algoithm for solving SAT, written in Haskell

It uses a Happy parser for parsing boolean CNF formula in [DIMACS](http://www.satcompetition.org/2009/format-benchmarks2009.html)

To run it simply do

```
cabal install alex
cabal install happy
cabal run < cnf
```