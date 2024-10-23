LABEL org.opencontainers.image.source=https://github.com/spex-lang/spex
LABEL org.opencontainers.image.description="Spex's static binary build image"
LABEL org.opencontainers.image.licenses=BSD-2-Clause

FROM ghcr.io/spex-lang/spex-build

RUN apk upgrade --no-cache \
  && apk add --no-cache \
    gcc \
    git \
    gmp-dev \
    libffi-dev \
    musl-dev \
    upx \
    zlib-dev \
    zlib-static

COPY --from=ghcr.io/spex-lang/spex-build \
  /root/.cache/cabal /root/.cache/cabal
COPY --from=ghcr.io/spex-lang/spex-build \
  /root/.local/state/cabal/store /root/.local/state/cabal/store 

RUN cabal build exe:spex exe:spex-demo-petstore \
      --enable-executable-static \
  && find ./dist-newstyle -name 'spex*' -type f -executable \
       -exec cp {} /usr/local/bin \; 
  # && upx -q /usr/local/bin/*
  # XXX: Use --best flag?

ENTRYPOINT [ "/bin/sh" ]
