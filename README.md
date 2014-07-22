# NICTA t3as Named-Entity Recognition (NER)

NICTA t3as Named-Entity Recognition is a rule based Named-Entity Recognition library which extracts named entities from text, such as Organisation, Location and Person names. It is written in Java.

This is currently a work-in-progress - so far it has mostly just been cleaned up and a simple web service and client have been added. Some of the things that still need to be completed:

* update all the word lists
* add a simple demo web front end for people to evaluate the library
* new features, and lots of tweaking of the existing features.


## Contributed by:

- Mats Henrikson
- William Han
- Scott Sanner
- Kishor Gawande


## License

Copyright (c) 2010, National ICT Australia
All rights reserved.

This software is under the GPL version 3.
Please see the license file LICENSE.txt

The contents (data/documentation/artwork) is under the Creative Commons 3.0 BY-SA 
More information about "Creative Commons" license can be found at
http://creativecommons.org/licenses/by-sa/3.0/


## Modules

The NICTA NER project contains a number of subprojects implementing various functionality:

1. [NICTA NER library](nicta-ner) This is the core NER library that does the actual processing work. This can be run from the command line or imported into a JVM based project.
2. [NER webservice](nicta-ner-web) A simple webservice that wraps the NER library.
3. [NER client](nicta-ner-client) A simple Java client for the remote webservice that can be used either from the command line, or by importing the Java library in your own JVM based projects.
