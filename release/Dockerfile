FROM ghcr.io/spex-lang/spex-build AS build

LABEL org.opencontainers.image.source=https://github.com/spex-lang/spex
LABEL org.opencontainers.image.description="Spex's static binary build image"
LABEL org.opencontainers.image.licenses=BSD-2-Clause

RUN apk upgrade --no-cache \
  && apk add --no-cache \
    gcc \
    musl-dev \
    upx 

RUN --mount=type=cache,target=/root/.local/state/cabal/store \
    --mount=type=cache,target=/root/.cache/cabal/packages \
    --mount=type=cache,target=dist-newstyle \
  cabal build exe:spex exe:spex-demo-petstore \
    --enable-executable-static \
  && find ./dist-newstyle -name 'spex*' -type f -executable \
       -exec cp {} /usr/local/bin \; \
  && upx -q /usr/local/bin/*
  # XXX: upx --best?

FROM scratch AS release

COPY --from=build /usr/local/bin .

ENTRYPOINT [ "/spex" ]
