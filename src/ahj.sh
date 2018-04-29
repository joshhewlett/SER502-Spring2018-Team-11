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
if [ ! -f $SCRIPTPATH/tokenizer.pl ]; then 
    echo -e "${RED}\$AHJ_HOME path variable not set correctly!${NC}"
    echo "Please make sure to set \$AHJ_HOME to full path of the /src directory of this project."
    exit 1
fi

# Global variables
dir='./ahjc/'
file=''
keepDirFlag=false
verboseFlag=false ### True by default for now


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
betterParseFile=$file".betterparse"

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
    echo -e "${YELLOW}Parsing tokens...${NC}"
fi

# Tokenize $betterParseFile and dump to $tokensFile
parseTreeFile=$dir"parseTree.ahjpt"
$(swipl -s $SCRIPTPATH/parser.pl $dir$betterParseFile > $dir"_out") &

# Wait for 2 seconds before killing program
sleep 0.1
kill -9 $!

# Get output
OUTPUT=$(cat $dir"_out")

# Get each program output

TOKENIZER_OUTPUT=$(echo $OUTPUT | awk -F " :-:-: " '{print $1}')
PARSE_TREE_OUTPUT=$(echo $OUTPUT | awk -F " :-:-: " '{print $2}')
PROGRAM_OUTPUT=$(echo $OUTPUT | awk -F " :-:-: " '{print $3}')
ERROR_MESSAGE=$(echo $OUTPUT | awk -F " :-:-: " '{print $4}')

if [ ! "$ERROR_MESSAGE" = '' ] ; then
    echo -e "${RED}Error: ${NC} Syntax error!"
    exit 1
fi

# Write program output to files
echo $TOKENIZER_OUTPUT > $dir$file".tokens"
echo $PARSE_TREE_OUTPUT > $dir$file".parse"
echo $PROGRAM_OUTPUT > $dir$file".output"

# Logs
if [ "$verboseFlag" = true ] ; then
    echo ""
    echo -e "${GREEN}Successfully executed program${NC}"
    echo -e "\n${YELLOW}Tokens: ${NC}$TOKENIZER_OUTPUT\n\n"
    echo -e "\n${YELLOW}Parse Tree: ${NC}$PARSE_TREE_OUTPUT\n\n"
fi

echo -e "\n${YELLOW}Program output: ${NC}"
echo -e ${PROGRAM_OUTPUT/' '/'\n'}

# If keepDirFlag=true, keep the dump directory.
# Delete if not.
if [ $keepDirFlag = false ] ; then
    rm -R $dir
fi
