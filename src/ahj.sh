#!/bin/bash

# Color codes for better logging
RED='\033[0;31m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
GREEN='\033[1;32m'
NC='\033[0m'

# Absolute path of this script
# SCRIPTPATH=$( dirname $(realpath -s $0))
SCRIPTPATH=$AHJ_HOME
echo "\$AHJ_HOME=$SCRIPTPATH"
if [ ! -f $SCRIPTPATH/tokenizer.pl ]; then 
    echo -e "${RED}\$AHJ_HOME path variable not set correctly!${NC}"
    echo "Please make sure to set \$AHJ_HOME to full path of the /src directory of this project."
    exit 1
fi

# Global variables
dir='./ahjc/'
file=''
betterParsePrefix='betterParse_'
keepDirFlag=false
verboseFlag=true ### True by default for now


# Get flags/arguments
while getopts 'kvhd:f:' flag; do
    case "${flag}" in
        d) dir="./${OPTARG}/" ;; # Directory argument
        f) file="${OPTARG}" ;; # File to compile argument
        k) keepDirFlag=true;; # Keep dump directory flag
        v) verboseFlag=true;; # Verbose - Log throughout script
        h) echo ""
            echo -e "${YELLOW}AH-J Compiler${NC}"
            echo -e "${YELLOW}Team 11${NC}"
            echo ""
            echo "Options: "
            echo "-h        Show help"
            echo -e "-f        ${RED}REQUIRED${NC} File to compile (must be .ahj file)"
            echo "-d        Directory to dump generated files (./ahjc by default)"
            echo "-k=FLAG   Keep generated files"
            echo "-v=FLAG   Show logs"
            echo ""
            exit 0
            ;;
        *) error 
            echo -e "${RED}Unexpected option ${flag}${NC}"
            exit 0
            ;;
    esac
done

# If .ahj file not provided, print error and exit
if [ "$file" = '' ] || [ ! ${file##*.} = 'ahj' ]; then
    echo -e "${RED}ERROR:${NC} Must provide .ahj file to compile"
    echo "See HELP (-h) for usage"
    exit 0
fi

### Create tmp directory and files

# Create new filename for dumping new file to parse
betterParseFile=$betterParsePrefix$file

# Logs
if [ "$verboseFlag" = true ] ; then
    echo ""
    echo -e "${YELLOW}Input file:${NC} $file"
    echo -e "${YELLOW}Converting${NC} '$file' ${YELLOW}to${NC} '$betterParseFile'"
    echo -e "${YELLOW}Dumping${NC} '$betterParseFile' ${YELLOW}to${NC} '$dir'"
fi

# Remove tmp directory if exists
if [ -d $dir ]; then
    rm -R $dir
fi

# Create directory and file
mkdir $dir
touch $dir$betterParseFile

# Read from argument file and replace all occurences of '.' with ' . '
while IFS='' read -r line || [[ -n "$line" ]]; do
    string=${line/'.'/' . '}
    echo "$string" >> $dir$betterParseFile

done < "$file"

# Logs
if [ "$verboseFlag" = true ] ; then
    echo ""
    echo -e "${YELLOW}Tokenizing $betterParseFile...${NC}"
fi

# Tokenize $betterParseFile and dump to $tokensFile
tokensFile=$dir"tokens.ahjt"
swipl -s $SCRIPTPATH/tokenizer.pl $dir$betterParseFile > $tokensFile

# Logs
if [ "$verboseFlag" = true ] ; then
    echo ""
    echo -e "${GREEN}Successfully created tokens${NC}"
    echo -e "\n${YELLOW}List of tokens: ${NC}"
    cat $tokensFile
fi

#Interpret


# If keepDirFlag=true, keep the dump directory.
# Delete if not.
if [ $keepDirFlag = false ] ; then
    rm -R $dir
fi
