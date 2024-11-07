FROM ghcr.io/spex-lang/spex-build AS build

LABEL org.opencontainers.image.source=https://github.com/spex-lang/spex
LABEL org.opencontainers.image.description="Spex's static binary build image"
LABEL org.opencontainers.image.licenses=BSD-2-Clause

# COPY spex.cabal cabal.project cabal.project.freeze example/*/*.cabal .
# 
# # https://docs.docker.com/reference/dockerfile/#run---mount
# RUN --mount=type=cache,target=/root/.local/state/cabal/store \
#     --mount=type=cache,target=/root/.cache/cabal \
#     --mount=type=cache,target=dist-newstyle \
#   cabal configure \
#     --enable-executable-static \
#     --disable-profiling \
#     --disable-library-for-ghci \
#     --enable-library-stripping \
#     --enable-executable-stripping \
#     --enable-tests \
#     --enable-benchmarks \
#     --disable-documentation \
#   && cabal update \
#   && cabal build all --only-dependencies
# 
# COPY . .
# 
# # We copied example/*/*.cabal into the working directory to build all
# # dependencies, but now we have all those cabal files there, in addition to
# # where they originally were inside the examples folder, so we have to remove
# # them.
# RUN --mount=type=secret,id=version \
#     --mount=type=cache,target=/root/.local/state/cabal/store \
#     --mount=type=cache,target=/root/.cache/cabal \
#     --mount=type=cache,target=dist-newstyle \
#   find . -maxdepth 1 \( -name '*.cabal' -a ! -name spex.cabal \) -delete \
#   && cabal build lib:spex lib:petstore \
#   && cabal test all

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
