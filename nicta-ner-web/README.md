# NICTA t3as NER Webservice

A simple Java Servlet based web service wrapping the NICTA NER library.

To build a WAR file simply run:

    mvn

To run a local Tomcat server with the web service run:

    mvn tomcat7:run


## Using the service

To use the service simply HTTP `POST` some `application/x-www-form-urlencoded` encoded `UTF-8` text to the service URL, usually (unless using a customised service):

    http://ner.t3as.org/nicta-ner-web/rest/v1.0/ner


## JSON response format

The service will respond with `application/json` content of the following format:

    {
        "phrases": [
            [
                {
                    "attachedWordMap": {},
                    "isDate": false,
                    "phrase": [
                        {
                            "startIndex": 0,
                            "text": "John"
                        },
                        {
                            "startIndex": 5,
                            "text": "Doe"
                        }
                    ],
                    "phraseLength": 2,
                    "phrasePosition": 0,
                    "phraseStubLength": 2,
                    "phraseStubPosition": 0,
                    "phraseType": "PERSON",
                    "score": [ 11.25, 35.0, 0.0 ]
                }
            ]
        ],
        "tokens": [
            [
                {
                    "startIndex": 0,
                    "text": "John"
                },
                {
                    "startIndex": 5,
                    "text": "Doe"
                }
            ]
        ]
    }


## NER classes

the NICTA t3as Named-Entity Recognition library currently returns result classified into one of these classes:

    UNKNOWN
    PERSON
    ORGANIZATION
    LOCATION
    DATE
