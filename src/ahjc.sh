#!/bin/bash


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
            echo "AH-J Compiler"
            echo "Team 11"
            echo ""
            echo "Options: "
            echo "-h        Show help"
            echo "-f        File to compile (must be .ahj file)"
            echo "-d        Directory to dump generated files"
            echo "-k=FLAG   Keep generated files"
            echo "-v=FLAG   Show logs"
            echo ""
            exit 0
            ;;
        *) error "Unexpected option ${flag}"
            exit 0
            ;;
    esac
done

# If .ahj file not provided, print error and exit
if [ "$file" = '' ] || [ ! ${file##*.} = 'ahj' ]; then
    echo "ERROR: Must provide .ahj file to compile"
    echo "See HELP (-h) for usage"
    exit 0
fi

### Create tmp directory and files

# Create new filename for dumping new file to parse
betterParseFile=$betterParsePrefix$file

# Logs
if [ "$verboseFlag" = true ] ; then
    echo ""
    echo "Tokenizing file: $file"
    echo "Converting '$file' to '$betterParseFile'"
    echo "Dumping '$betterParseFile' to '$dir'"
    echo ""
fi

# Remove tmp directory if exists
if [ -d $dir ]; then
    rm -R $dir
fi

# Create directory and file
mkdir $dir
touch $betterParseFile

# Read from argument file and replace all occurences of '.' with ' . '
while IFS='' read -r line || [[ -n "$line" ]]; do
    string=${line/'.'/' . '}
    echo $string >> $dir$betterParseFile

done < "$file"

# Parse and wait until process is finished
# TODO pass verbose flag into prolog for logging as well
swipl -s template.pl $dir$betterParseFile &
wait $!

if [ "$verboseFlag" = true ] ; then
    echo ""
    echo "Finished creating tokens"
    echo ""
fi

#Interpret


# If keepDirFlag=true, keep the dump directory.
# Delete if not.
if [ $keepDirFlag = false ] ; then
    rm -R $dir
    rm ./$betterParseFile
fi