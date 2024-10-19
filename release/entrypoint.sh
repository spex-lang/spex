#!/bin/sh

cd /mnt \
    && cabal update \
    && cabal build exe:spex exe:spex-demo-petstore --enable-executable-static 
