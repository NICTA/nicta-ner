NICTA Named Entity Recognizer (NER)
November 2010
================================

NICTA Named Entity Recognizer is a java based Named Entity Recognizer which
extracts named entities from text such as Organization, Location and Person
names.

Contributed by:
================================
- William Han (william.han@nicta.com.au)
- Scott Sanner (scott.sannar@nicta.com.au)
- Kishor Gawande (kishor.gawande@nicta.com.au)


************************************************************
DISCLAIMER:

NICTA is not liable to you or anyone else if interference with or damage
to your computer systems occurs in connection with use of this NER
software. 

************************************************************

LICENSE
================================

Copyright (c) 2010, National ICT Australia
All rights reserved.

This software is under the MOZILLA PUBLIC LICENSE (MPL).
Please see the license file LICENSE.txt

The contents (data/documentation/artwork) is under the Creative Commons 3.0 BY-SA 
More information about "Creative Commons" license can be found at
http://creativecommons.org/licenses/by-sa/3.0/

QUICK START
================================

- Command line shell interface to process the input text

  > java -Xmx200m -jar NamedEntityAnalyser.jar
  Type in texts, -q for quit.
  > <Input text>   
  
- Find named entities from the text document:

  > java -Xmx200m -jar NamedEntityAnalyser.jar <text fileName>  

- Using jar in the java code:
  
  // import ner pakcage
  import nicta.ner.NamedEntityAnalyzer;
  import nicta.ner.NERResultSet;

  // Create analyser
  NamedEntityAnalyzer namedEntityAnalyzer = new NamedEntityAnalyzer();

  // string analysis
  NERResultSet nerResultSet = namedEntityAnalyzer.process("String input here.");
 
  // file analysis
  NERResultSet nerResultSet = namedEntityAnalyzer.process(NamedEntityAnalyzer.ReadFileAsString("File name"));
  
  // output to string
  String result = nerResultSet.toString();

  // output to HashMap
  HashMap<String, String> resultMap = nerResultSet.getMappedResult();


CONTACT
================================
For questions about this distribution, please contact NICTA ADA (Automated Data Analysis) Group
- William Han (william.han@nicta.com.au)
- Scott Sanner (scott.sannar@nicta.com.au)
- Kishor Gawande (kishor.gawande@nicta.com.au)


PACKAGE OUTLINE
================================
nicta.ner                        - The package which contains the main class:
                                   NamedEntityAnalyser; and the output result
                                   set: NERResultSet. The program follows the
                                   pipe-line:
                                   text ------> phrases ------> classified phrases
                                       extractor       classifier
                                   
nicta.ner.util                   - Utilities such as the tokenizer and the
                                   dictionary.
                                   
nicta.ner.data                   - Data structures which are used in the
                                   program.
                                   
nicta.ner.resource               - Configuration package. The configuration
                                   file controls the classifier. See
                                   nlp.nicta.ner.classifier
                                   
nicta.ner.extractor              - A rule based extractor which only extract
                                   phrases from plain texts.
                                   
nicta.ner.classifier             - Feature based classifier which is controlled
                                   by features in the feature package (see
                                   nlp.nicta.ner.classifier.feature) and the
                                   weight array specified in the configuration
                                   file.
                                   
nicta.ner.classifier.feature     - Features.

