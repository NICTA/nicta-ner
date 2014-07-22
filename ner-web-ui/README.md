ner-web-ui
==========

A small demo web user interface for testing the NICTA t3as Named-Entity Recognition software.

This UI is a webapp written in Haskell using the Yesod framework.


## Getting started

First, make sure you have `haskell-platform` installed for your OS, with a `cabal-install` version of 1.18 or higher. Run this command to check:

    cabal --version

Change into the directory of this README.md, and run the following commands:

    cabal sandbox init
    cabal install --enable-tests . yesod-platform yesod-bin --max-backjumps=-1 --reorder-goals -j3
    yesod devel


