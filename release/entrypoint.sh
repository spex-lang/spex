#!/bin/sh

# To avoid cabal update always refetching the latest packages, we pin it to a
# specific date. This can be removed once the following bug is fixed:
# https://github.com/haskell/cabal/issues/8572

cd /mnt \
    && cabal update hackage.haskell.org,2024-10-21T00:00:00Z \
    && cabal build all --only-dependencies --enable-static \
    && cabal build exe:spex exe:spex-demo-petstore --enable-static --enable-executable-static

    # We can't use cabal install here, because that causes --version to break
    # due to the following bug: https://github.com/acfoltzer/gitrev/issues/23
    #
    # && cabal install exe:spex exe:spex-demo-petstore \
    #        --enable-executable-static \
    #        --install-method=copy \
    #        --overwrite-policy=always \
    #        --installdir=/mnt/bin
