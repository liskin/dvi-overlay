#!/bin/bash

# Process both documents with latex and then combine labels into .aux files.

if [ $# != 2 ]; then
    echo "Usage: overlay.sh <a.tex> <b.tex>"
    exit 3
fi

if [ -z "$LATEX" ]; then
    LATEX=latex
fi

aux() {
    # The % at the end helps us distinguish between original and added entries.
    perl -ne 'if(/^(\\newlabel.*\{)(\S+\}\{\}\})$/){ print $1 . "'$2':" . $2 . "%\n" }' $1
    perl -ne 'if(/^(.*overlay@sync.*\})$/){ print $1 . "%\n" }' $1
}

I1=$1
I2=$2
A1=${1%.tex}.aux
A2=${2%.tex}.aux
L1=${1%.tex}.log
L2=${2%.tex}.log

iter() {
    $LATEX $I1
    $LATEX $I2
    aux $A1 doc1 >>$A2
    aux $A2 doc2 >>$A1
}

pages() {
    perl -ne 'if(/^Output written on .* \((\d+) pages,/){ print $1 }' $1
}

again() {
    egrep 'Rerun (LaTeX|to get cross-references right|to get citations correct)' $L1 $L2 || [ "`pages $L1`" != "`pages $L2`" ]
}

set -e

latex_count=20

iter
while again && [ $[latex_count--] -gt 0 ]; do
    iter
done
