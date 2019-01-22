#!/bin/bash

cd "$(dirname "$0")"

echo Kompiluję Przelewankę

ocamlc -g -w +a -c przelewanka.mli przelewanka.ml || exit 1
ocamlopt -c przelewanka.mli przelewanka.ml || exit 1

for f in tests/*.ml
do
    echo Przetwarzam: $(basename "$f")
    ocamlopt -g -w -a -c "$f" || exit 2
    ocamlopt -g -o "${f%%.*}" przelewanka.cmx "${f%%.*}".cmx || exit 3
    OCAMLRUNPARAM="b,l=100M" ./"${f%%.*}"
    rm "${f%%.*}" "${f%%.*}".cmx "${f%%.*}".o "${f%%.*}".cmi
    tput sgr0
done
