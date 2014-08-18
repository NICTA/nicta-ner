Extracting data from DBPedia
============================

DBPedia provides a structured view of the data from WikiPedia. Unfortunately, the data is of very bad quality and therefore requires a significant amount of cleanup.

## Scripts

A number of scripts automating the data retrieval and sanitation has been written:

* [CountryNames.sh](CountryNames.sh)
* [PopulatedPlaces.sh](PopulatedPlaces.sh)
* [OrganisationNames.sh](OrganisationNames.sh)
* [PersonNames.sh](PersonNames.sh)

When run these scripts generate a `Long<name>.dbpedia` and a `Short<name>.dbpedia` file. The `Long` file has all text lines which have a space in them. If we get a phrase match in here we can be a little more confident that it is correct. The `Short` file has all the text lines that have no spaces, so here we can only match single words, so we cannot be as confident they are correct.

Once downloaded put the files in the [../src/main/resources/org/t3as/ner/classifier/feature/](../src/main/resources/org/t3as/ner/classifier/feature/) directory.
