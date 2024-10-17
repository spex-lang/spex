#!/bin/sh

cd /mnt \
    && cabal update \
    && cabal build exe:spex --enable-executable-static 
