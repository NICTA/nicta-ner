ner-web-ui
==========

A small demo web user interface for testing the NICTA t3as Named-Entity Recognition software.

This UI is a webapp written in Haskell using the Yesod framework.


## Contributed by:

- Mats Henrikson


## License

Copyright (c) 2014, National ICT Australia
All rights reserved.

This software is under the GPL version 3.
Please see the license file LICENSE.txt


## Getting started

First, make sure you have `haskell-platform` installed for your OS, with a `cabal-install` version of 1.18 or higher. Run this command to check:

    cabal --version

Change into the directory of this README.md, and run the following commands (which will also build the nicta-ner-client-hs dependency):

    cabal sandbox init
    cabal install ../nicta-ner-client-hs/ . yesod-platform yesod-bin --max-backjumps=-1 --reorder-goals -j4
    yesod devel
