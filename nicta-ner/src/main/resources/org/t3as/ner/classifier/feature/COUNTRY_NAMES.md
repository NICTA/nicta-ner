Steps to generate the COUNTRY_NAME data
=======================================

## DBPedia query for CSV data
    PREFIX dbprop: <http://dbpedia.org/property/>
    SELECT DISTINCT ?name
    WHERE {
       ?country rdf:type dbpedia-owl:Country;
       dbprop:commonName ?name
    }
    ORDER BY ?name


## Further steps

### Automagic munging
Remove quotation marks:

    sed 's/"//g' sparql.csv > COUNTRY_NAMES.1
 
Replace ` or ` with newlines (tricky to do in OSX, requires actual newline in the regex):

    sed 's/ or /\
    > /g' COUNTRY_NAMES.1 > COUNTRY_NAMES.2

Remove lines longer than 60 columns (these are just random stuff people managed to stuff in this field):

    sed '/^.\{60,\}$/d' COUNTRY_NAMES.2 > COUNTRY_NAMES.3
    
Remove lines with commas as there is no simple sane way we can change these to work for us:

    grep -v , COUNTRY_NAMES.3 > COUNTRY_NAMES.4

Remove leading `the `:

    sed 's/^the \(.*\)/\1/' COUNTRY_NAMES.4 > COUNTRY_NAMES.5

Remove `name`:
  
    sed 's/^name$//' COUNTRY_NAMES.5 > COUNTRY_NAMES.6
    
Finally `sort` and `uniq`:

    sort COUNTRY_NAMES.7 | uniq > COUNTRY_NAMES

#### Manual editing

Add a `Creative Commons Attribution-Sharealike 3.0 Unported License (CC-BY-SA)`:

<https://creativecommons.org/licenses/by-sa/3.0/legalcode>
