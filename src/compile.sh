#!/bin/bash

### Create tmp directory and files

betterParsePrefix="betterParse_"
betterParseFile=$betterParsePrefix$1
echo $betterParseFile

# Remove tmp directory if exists
if [ -f $file ]; then
    rm -R tmp
fi

# Create directory and file
mkdir tmp
tmp='./tmp/'
touch $betterParseFile

# Read from argument file and replace all occurences of '.' with ' . '
while IFS='' read -r line || [[ -n "$line" ]]; do
    string=${line/'.'/' . '}
    echo $string >> $tmp$betterParseFile

done < "$1"

#Parse
swipl -s template.pl $tmp$betterParseFile

#Interpret
