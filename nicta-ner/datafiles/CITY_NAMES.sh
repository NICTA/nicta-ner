#!/bin/bash

LIMIT=40000
RANGE=`seq 0 $LIMIT 200000`

for f in $RANGE; do
    echo -n "Requesting City names from $f... "

    QUERY=$(python -c 'import sys,urllib;print urllib.quote(sys.stdin.read().strip())' <<EOF
SELECT ?name {{
    SELECT DISTINCT ?name
    WHERE {
      {
        ?x rdf:type dbpedia-owl:City;
        dbpprop:name ?name;
        rdfs:label ?label .
        FILTER (lang(?label) = 'en')
      }
    UNION
      {
        ?x rdf:type dbpedia-owl:Settlement;
        dbpprop:name ?name;
        rdfs:label ?label .
        FILTER (lang(?label) = 'en')
      }
    }
    ORDER BY ?name
}}
OFFSET $f
LIMIT $LIMIT
EOF)

    FILE=CITY_NAMES_partial-$f.csv
    curl -s -H "Accept: text/csv" "http://dbpedia.org/sparql?query=$QUERY" -o $FILE
    echo -n "Retrieved: "
    wc -l $FILE
done

# Concatenate all the results, removing quotation marks and any other char we don't want to deal with
cat CITY_NAMES_partial-*.csv | tr -d '"*' > CITY_NAMES_unfinished

# Replace &#39; with apostrophe
sed -Ei .1 "s/&#39;/'/g" CITY_NAMES_unfinished

# Remove lines with weird chars we can't do anything with
sed -Ei .2 '/[,\/;#]/d' CITY_NAMES_unfinished

# Remove lines being entirely numbers, starting with '-', and single character lines
sed -Ei .3 '/^[0-9]*$/d' CITY_NAMES_unfinished
sed -Ei .4 '/^-/d' CITY_NAMES_unfinished
sed -Ei .5 '/^.$/d' CITY_NAMES_unfinished

# Remove lines longer than 60 columns (these are just random stuff people managed to stuff in this field)
sed -i .6 '/^.\{60,\}$/d' CITY_NAMES_unfinished

# Remove `name`, and some other spurious lines
sed -Ei .6 "/^(name|74.0|' or ')\$/d" CITY_NAMES_unfinished

# Sort an uniq to the final result
sort < CITY_NAMES_unfinished | uniq > CITY_NAMES
