nicta-ner-client-hs
===================

A simple client for the NICTA t3as Named-Entity Recognition web service, written in Haskell.


## Contributed by:

- Mats Henrikson


## License

Copyright (c) 2014, National ICT Australia
All rights reserved.

This software is under the GPL version 3.
Please see the license file LICENSE.txt


## Getting started

Change into the directory of this README.md, and run the following commands:

    cabal sandbox init
    cabal install --only-dependencies -j4
    cabal install
    .cabal-sandbox/bin/nicta-ner-client-hs --help
