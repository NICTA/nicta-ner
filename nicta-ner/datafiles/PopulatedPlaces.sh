#!/bin/bash

TARGET=PopulatedPlaces.dbpedia
LIMIT=40000
RANGE=`seq 0 $LIMIT 480000`

for f in $RANGE; do
    echo -n "Requesting ${TARGET} from $f... "

    QUERY=$(python -c 'import sys,urllib;print urllib.quote(sys.stdin.read().strip())' <<EOF
        SELECT ?name {{
            SELECT DISTINCT ?name
            WHERE {
                ?x rdf:type dbpedia-owl:Place .
                ?x foaf:name ?name .
                FILTER (lang(?name) = 'en')
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

# Split into long and short
grep " " ${TARGET}_sanitised > Long${TARGET}_sanitised
grep -v " " ${TARGET}_sanitised > Short${TARGET}_sanitised

# Add the DBPedia header
cat dbpedia.header Long${TARGET}_sanitised > Long${TARGET}
cat dbpedia.header Short${TARGET}_sanitised > Short${TARGET}

echo "Finished files:"
wc -l Long${TARGET} Short${TARGET}
