#!/bin/bash

# Process both documents with latex and then combine labels into .aux files.

if [ $# != 2 ]; then
    echo "Usage: overlay.sh <a.tex> <b.tex>"
    exit 3
fi

if [ -z "$LATEX" ]; then
    LATEX=latex
fi

labels() {
    # The % at the end helps us distinguish between original and added entries.
    perl -ne 'if(/^(\\newlabel.*\{)(\S+\}\{\}\})$/){ print $1 . "'$2':" . $2 . "%\n" }' $1
}

A1=${1%.tex}.aux
A2=${2%.tex}.aux

$LATEX $1 && $LATEX $2 && labels $A1 doc1 >>$A2 && labels $A2 doc2 >>$A1
