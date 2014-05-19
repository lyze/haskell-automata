Haskell Automata
================

Saajid Moyen and David Xu

We wrote an interface and implemented canonical algorithms for NFAs and DFAs.

The conversion between direct and indirect automata was perhaps the most
interesting piece of code to write because it "ties the knot."

GADTs are used to hide the typeclass constraints, making certain function
signatures look nicer.
