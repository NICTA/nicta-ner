# NICTA t3as Named-Entity Recognition (NER)

NICTA t3as Named-Entity Recognition is a rule based Named-Entity Recognition library which extracts named entities from text, such as Organisation, Location and Person names. It is written in Java.

This currently has a fairly long startup time due to the large amount of data that has to be loaded into memory. It is therefore recommended that the [web service](../nicta-ner-web/) is used, and one of the clients (or even just `curl`) is invoked repeatedly.

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


## Maven Central

This project is also available on Maven Central under the `org.t3as` group id, as artifact id `snomedct-lookup`:

<http://search.maven.org/#search%7Cga%7C1%7Ca%3A%22snomedct-lookup%22>


## NER data files

The NICTA t3as NER library makes use of rules and lists of known entities to detect entities in free text. These lists of entities are extracted from different sources. Two of the main sources are [DBPedia](http://dbpedia.org/) and [Freebase](http://www.freebase.com/). More information about how to get this data can be found below:

* [DbpediaDataFiles](datafiles/DbpediaDataFiles.md)
* [FreebaseDataFile](datafiles/FreebaseDataFiles.md)


## Quick start

Command line shell interface to process the input text

        > java -Xmx200m -jar NamedEntityAnalyser.jar
        Type in texts, -q for quit.
        > <Input text>   
  
Find named entities from the text document:

        > java -Xmx200m -jar NamedEntityAnalyser.jar <text fileName>  

Using jar in the java code:
  
        // import ner package
        import nicta.ner.NamedEntityAnalyser;
        import nicta.ner.NERResultSet;

        // Create analyser
        NamedEntityAnalyser namedEntityAnalyzer = new NamedEntityAnalyser();

        // string analysis
        NERResultSet nerResultSet = namedEntityAnalyzer.process("String input here.");
 
        // file analysis
        NERResultSet nerResultSet = namedEntityAnalyzer.process(NamedEntityAnalyser.ReadFileAsString("File name"));
  
        // output to string
        String result = nerResultSet.toString();

        // output to HashMap
        HashMap<String, String> resultMap = nerResultSet.getMappedResult();


## NER classes

the NICTA t3as Named-Entity Recognition library currently returns result classified into one of these classes:

    UNKNOWN
    PERSON
    ORGANIZATION
    LOCATION
    DATE


## Package outline

### nicta.ner                        
The package which contains the main class `NamedEntityAnalyser` and the output result set `NERResultSet`. The program follows the pipe-line:
    
    text ------> phrases ------> classified phrases
        extractor       classifier
                                   
### nicta.ner.util                   
Utilities such as the tokenizer and the dictionary.
                                   
### nicta.ner.data                   
Data structures which are used in the program.
                                   
### nicta.ner.resource               
Configuration package. The configuration file controls the classifier. See `nlp.nicta.ner.classifier`
                                   
### nicta.ner.extractor              
A rule based extractor which only extract phrases from plain texts.
                                   
### nicta.ner.classifier             
Feature based classifier which is controlled by features in the feature package (see `nlp.nicta.ner.classifier.feature`) and the weight array specified in the configuration file.
                                   
### nicta.ner.classifier.feature     
Features.

