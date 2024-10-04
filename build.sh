#!/bin/zsh
mkdir build && ghc -package split -package containers -o build/TextAdventure TextAdventure.hs -hidir build -odir build