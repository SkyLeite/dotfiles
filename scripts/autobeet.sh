#!/usr/bin/env bash

MUSIC_LOCATION=~/Deezloader

while true; do
find $MUSIC_LOCATION | entr -d beet import $MUSIC_LOCATION --quiet --move
done
