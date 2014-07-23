# NICTA t3as NER Client

A simple Java based client for the remote NER web service.


## Contributed by:

- Mats Henrikson


## License

Copyright (c) 2014, National ICT Australia
All rights reserved.

This software is under the GPL version 3.
Please see the license file LICENSE.txt

The contents (data/documentation/artwork) is under the Creative Commons 3.0 BY-SA 
More information about "Creative Commons" license can be found at
http://creativecommons.org/licenses/by-sa/3.0/


## Maven Central

This project is also available on Maven Central under the `org.t3as` group id, as artifact id `snomedct-lookup`:

<http://search.maven.org/#search%7Cga%7C1%7Ca%3A%22snomedct-lookup%22>


To run the client from the command line first build it using Maven:

    mvn

You can then run it using the `nicta-ner-client` script:

    ./nicta-ner-client -text "Text to NER goes here."
    ./nicta-ner-client /some/file/with/text.txt /some/other/file.txt

To query a [NER web service](../nicta-ner-web) different from the one at <http://ner.t3as.org/> (NOTE: NOT CURRENTLY RUNNING!) then also pass the `-url` option:
 
    ./nicta-ner-client -url http://localhost:8080/nicta-ner-web/ -text "NER me."
    
To see a full list of options pass the `-h` option:

    ./nicta-ner-client -h
