#!/bin/bash

cd "$(dirname "$0")"

echo Kompiluję Origami

ocamlc -c origami.mli origami.ml || exit 1

for f in tests/*.ml
do
    echo Przetwarzam: $(basename "$f")
    ocamlc -c "$f" || exit 2
    ocamlc -o "${f%%.*}" origami.cmo "${f%%.*}".cmo || exit 3
    time ./"${f%%.*}"
    rm "${f%%.*}" "${f%%.*}".cmo "${f%%.*}".cmi
done
