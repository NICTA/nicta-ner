Extracting data from a Freebase data dump
=========================================

The Freebase data dumps can be downloaded from here:

<https://developers.google.com/freebase/data>


## Country data

Start by extracting all the country data together to an easier to deal with file (you can skip the `pv` and just `gzcat` straight from the file, but `pv` adds a progress meter). `perl` is around 10x faster than `grep` and so it recommended on this volume of data:

    pv < freebase-rdf-2014-08-10-00-00.gz | gzcat | perl -ne '/location\.country\./ && print' > location.country.freebase

### Country codes:

    cut -f2,3 location.country.freebase | grep -E '(iso3166_1_alpha2|iso_alpha_3|fips10_4|fifa_code)' > country_codes.freebase
    
### Country names:

    cut -f2,3 location.country.freebase | grep iso3166_1_shortname | grep -v true > country_names.freebase

### Cleanup of the files:

    cat <filename> | cut -f 2 | sed -E 's/["\\]//g' | sed -E 's/,.*$//' | sed -E '/[a-z]/d' |  sort | uniq > <newfile>


## License

Put the following license header at the top of each of the files:

    #
    # This data has been extracted from the Freebase data dumps, available here:
    # 
    # http://www.freebase.com/
    # https://developers.google.com/freebase/data
    #
    # The Freebase data is licensed under the Creative Commons Attribution 2.5 Generic (CC BY 2.5) license, available here:
    #
    # http://creativecommons.org/licenses/by/2.5/legalcode

