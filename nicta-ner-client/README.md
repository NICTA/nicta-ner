# NICTA t3as NER Client

A simple Java based client for the remote NER web service.

To run the client from the command line first build it using Maven:

    mvn

You can then run it using the `nicta-ner-client` script:

    ./nicta-ner-client -text "Text to NER goes here."
    ./nicta-ner-client /some/file/with/text.txt /some/other/file.txt

To query a [NER web service](../nicta-ner-web) different from the one at <http://ner.t3as.org/> (NOTE: NOT CURRENTLY RUNNING!) then also pass the `-url` option:
 
    ./nicta-ner-client -url http://localhost:8080/nicta-ner-web/ -text "NER me."
    
To see a full list of options pass the `-h` option:

    ./nicta-ner-client -h
