#!/bin/bash

TARGET=PERSON_NAMES
LIMIT=40000
RANGE=`seq 0 $LIMIT 900000`

for f in $RANGE; do
    echo -n "Requesting ${TARGET} from $f... "

    QUERY=$(python -c 'import sys,urllib;print urllib.quote(sys.stdin.read().strip())' <<EOF
SELECT ?name {{
    SELECT DISTINCT ?name
    WHERE {
       ?x rdf:type dbpedia-owl:Person;
       foaf:name ?name
    }
    ORDER BY ?name
}}
OFFSET $f
LIMIT $LIMIT
EOF)

    FILE=${TARGET}_partial-$f.csv
    curl -s -H "Accept: text/csv" "http://dbpedia.org/sparql?query=$QUERY" -o $FILE
    echo -n "Retrieved: "
    wc -l $FILE
done

# Concatenate all the results, removing quotation marks and any other char we don't want to deal with
cat ${TARGET}_partial-*.csv > ${TARGET}_unsanitised

echo "Sanitising file:"
wc -l ${TARGET}_unsanitised

# Sanitise stuff we don't want to keep
./sanitise.sh ${TARGET}_unsanitised ${TARGET}_sanitised

# Remove lines that start with a number
sed -Ei .sed '/^[0-9]/d' ${TARGET}_sanitised

# Add the DBPedia header
cat dbpedia.header ${TARGET}_sanitised > ${TARGET}

echo "Finished file:"
wc -l ${TARGET}
