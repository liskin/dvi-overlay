#!/bin/bash

# Overlay two DVI files.

if [ $# != 3 ]; then
    echo "Usage: dvi-overlay.sh <a.dvi> <b.dvi> <out.dvi>"
    exit 3
fi

set -e

dviasm -o $1.dasm $1
dviasm -o $2.dasm $2
./DviOverlay $1.dasm $2.dasm $3.dasm
dviasm -o $3 $3.dasm
