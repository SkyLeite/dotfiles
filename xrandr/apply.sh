#!/bin/sh
if [ -z "$1" ]; then
    echo "Please pass your layouts directory"
fi

if [ $(hostname) = 'rodrigo-Inspiron-3647' ]; then
    "$1"/work.sh
fi
