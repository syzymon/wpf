#!/bin/bash

cd "$(dirname "$0")"

echo Kompiluję Sortowanie topologiczne

ocamlc -g -c pMap.mli pMap.ml topol.mli topol.ml || exit 1

for f in tests/*.ml
do
    echo Przetwarzam: $(basename "$f")
    ocamlc -g -c "$f" || exit 2
    ocamlc -g -o "${f%%.*}" pMap.cmo topol.cmo "${f%%.*}".cmo || exit 3
    time OCAMLRUNPARAM="b,l=100M" ./"${f%%.*}"
    rm "${f%%.*}" "${f%%.*}".cmo "${f%%.*}".cmi
done
rm pMap.cmi pMap.cmo
