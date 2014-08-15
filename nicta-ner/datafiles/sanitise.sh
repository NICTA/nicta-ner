#!/bin/bash

# A simple script to remove just from a file of things.
#
# Pass two parameters, input file to sanitise and output file to put the data
# into (this will be overwritten if it exists.)

FILE=$1
F=$FILE.sanitising
FINISHED=$2

# Remove quotation marks and any other char we don't want to deal with
tr -d '"*' < $FILE > $F

# Replace &#39; with apostrophe
sed -Ei .sed "s/&#39;/'/g" $F

# Lines with no alphabet characters at all (i.e. '=' or all number lines)
sed -Ei .sed '/^[^[:alpha:]]+$/d' $F

# Remove lines which include weird chars we can't do anything with
sed -Ei .sed '/[,\/:;#|{}()~$%!?=+]/d' $F

# Remove lines that start with &, '', -
sed -Ei .sed "/^(\&|''|-)/d" $F

# Remove single character lines
sed -Ei .sed '/^.$/d' $F

# Remove lines longer than 60 columns (usually just random stuff people managed to stuff in this field)
sed -i .sed '/^.\{60,\}$/d' $F

# Remove lines 1 or 2 characters long
sed -i .sed '/^.\{0,2\}$/d' $F

# Remove `name`, "' or '", '[0-9]px'
sed -Ei .sed "/^(name|' or ')\$/d" $F
sed -Ei .sed '/[0-9]px/d' $F

# Remove all leading lower case words, or complete lowercase lines, (that may also contain spaces, hypens, and dots)
sed -Ei .sed 's/^[-a-z\ .]+//' $F

# Sort and uniq to the final result
sort < $F | uniq > $FINISHED
