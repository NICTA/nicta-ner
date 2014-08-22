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


## Organisation data

### Stock symbols

First grab all the stock symbols out of the Freebase dump:

    pv < freebase-rdf-2014-08-10-00-00.gz | gzcat | perl -ne '/business.stock_ticker_symbol.ticker_symbol/ && print' > business.stock_ticker_symbol.ticker_symbol
    
Then grab all the symbols out:

    cut -f3 < business.stock_ticker_symbol.ticker_symbol | sed -E 's/"(.+)".*/\1/' | sed -E '/^[^A-Z]/d' | sed '/^.\{0,2\}$/d' | sort | uniq > StockSymbols.freebase

The `sed` commands keep only the part between the double quotes, drops anything that doesn't start with a capital letter, drops lines between 1 and 2 characters long.

### Organisation names

Do the first two steps together:

    pv < freebase-rdf-2014-08-10-00-00.gz | gzcat | perl -ne '/organization\.organization/ && print' > organization.organization
    pv < freebase-rdf-2014-08-10-00-00.gz | gzcat | perl -ne '/type\.object\.name/ && print' > type.object.name

Extract the org ids (can be combined with the step above, but I already had that list by then):

    perl -ne '/\/organization\.organization>/ && print' < organization.organization | perl -ne '/\/type\.object\.type>/ && print' > org_ids
    
Get only the English names out:

    pv < type.object.name | perl -ne '/\@en/ && print' > type.object.name.en
    
Make a file of just the entity id and the name string:

    pv < type.object.name.en | perl -ne '/com\/ns\/(m\..+?)>.*"(.+)"/ && print "$1\t$2\n"' > id_name

Do something similar to get just the org ids we are interested in:

    pv < org_ids | perl -ne '/com\/ns\/(m\..+?)>/ && print "$1\n"' > just_org_ids
    
Then use hacky java to get the names out:

    java -Xmx12g OrgIdNameLookup just_org_ids id_name > OrganisationNames.freebase
    
Finish processing into files:

    sort < org_names | uniq | sed '/^.\{0,2\}$/d' > OrganisationNames.freebase
    grep " " OrganisationNames.freebase > LongOrganisationNames.freebase
    grep -v " " OrganisationNames.freebase > ShortOrganisationNames.freebase
    
    

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

