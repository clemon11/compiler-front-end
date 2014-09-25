#!/bin/bash

if ! [ -x intermediateGeneration/main ]; then
  echo "Binary not found. Attemping to compile."
  cd intermediateGeneration
  ghc *.hs
  cd ..
fi;

intermediateGeneration/main $@
