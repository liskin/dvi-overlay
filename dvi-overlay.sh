#!/bin/bash

# Overlay two DVI files.

if [ $# != 3 ]; then
    echo "Usage: dvi-overlay.sh <a.dvi> <b.dvi> <out.dvi>"
    exit 3
fi

set -e

dviasm $1 >$1.dasm
dviasm $2 >$2.dasm
./DviOverlay $1.dasm $2.dasm $3.dasm
dviasm $3.dasm >$3
