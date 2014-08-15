#!/bin/bash

TARGET=COUNTRY_NAMES

echo -n "Requesting ${TARGET} from $f... "

QUERY=$(python -c 'import sys,urllib;print urllib.quote(sys.stdin.read().strip())' <<EOF
SELECT DISTINCT ?name
WHERE {
    ?country rdf:type dbpedia-owl:Country;
    foaf:name ?name;
    rdfs:label ?label .
    FILTER (lang(?label) = 'en')
}
ORDER BY ?name
EOF)

FILE=${TARGET}_partial-$f.csv
curl -s -H "Accept: text/csv" "http://dbpedia.org/sparql?query=$QUERY" -o $FILE
echo -n "Retrieved: "
wc -l $FILE

# Concatenate all the results, removing quotation marks and any other char we don't want to deal with
cat ${TARGET}_partial-*.csv > ${TARGET}_unsanitised

echo "Sanitising file:"
wc -l ${TARGET}_unsanitised

# Sanitise stuff we don't want to keep
./sanitise.sh ${TARGET}_unsanitised ${TARGET}_sanitised

# Add the DBPedia header
cat dbpedia.header ${TARGET}_sanitised > ${TARGET}

echo "Finished file:"
wc -l ${TARGET}
