ARG GHC_VERSION=9.6.6
ARG CABAL_VERSION=3.12.1.0

FROM docker.io/library/alpine:3.20 AS build

ARG GHC_VERSION
ARG CABAL_VERSION

LABEL org.opencontainers.image.source=https://github.com/spex-lang/spex
LABEL org.opencontainers.image.description="Spex's build and test image"
LABEL org.opencontainers.image.licenses=BSD-2-Clause

# Install system dependencies needed for ghcup.
# https://www.haskell.org/ghcup/install/#system-requirements
RUN apk upgrade --no-cache \
  && apk add --no-cache \
      binutils-gold \
      curl \
      gcc \
      g++ \
      gmp-dev \
      gpg \
      gpg-agent \
      libc-dev \
      libffi-dev \
      make \
      musl-dev \
      ncurses-dev \
      perl \
      tar \
      xz

# Install ghcup as per:
# https://www.haskell.org/ghcup/install/#manual-installation
RUN curl --proto '=https' --tlsv1.2 --silent --show-error --fail \
         --output /bin/ghcup \
         'https://downloads.haskell.org/ghcup/x86_64-linux-ghcup' \
  && chmod 0755 /bin/ghcup \
  && gpg --batch --keyserver keyserver.ubuntu.com \
         --recv-keys 7D1E8AFD1D4A16D71FADA2F2CCC85C0E40C06A8C \
  && gpg --batch --keyserver keyserver.ubuntu.com \
         --recv-keys FE5AB6C91FEA597C3B31180B73EDE9E8CFBAEF01 \
  && gpg --batch --keyserver keyserver.ubuntu.com \
         --recv-keys 88B57FCF7DB53B4DB3BFA4B1588764FBE22D19C4 \
  && gpg --batch --keyserver keyserver.ubuntu.com \
         --recv-keys EAF2A9A722C0C96F2B431CA511AAD8CEDEE0CAEF \
  && ghcup install cabal $CABAL_VERSION --set \
  && ghcup install ghc $GHC_VERSION --set

# Remove a bunch of stuff that we won't need.
# See https://stackoverflow.com/questions/4858585/why-is-ghc-so-large-big
RUN  rm -r /root/.ghcup/cache \
  && rm -r /root/.ghcup/db \
  && rm -r /root/.ghcup/hls \
  && rm -r /root/.ghcup/logs \
  && rm -r /root/.ghcup/share \
  && rm -r /root/.ghcup/tmp \
  && rm -r /root/.ghcup/trash \
  && rm -r /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/bin/ghc-iserv-dyn-ghc-$GHC_VERSION \
  && rm -r /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/bin/ghc-iserv-ghc-$GHC_VERSION \
  && rm -r /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/bin/ghc-iserv-prof-ghc-$GHC_VERSION \
  && rm -r /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/bin/haddock-ghc-$GHC_VERSION \
  && rm -r /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/bin/hp2ps-ghc-$GHC_VERSION \
  && rm -r /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/bin/hpc-ghc-$GHC_VERSION \
  && rm -r /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/bin/unlit-ghc-$GHC_VERSION \
  && rm -r /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/lib/bin \
  && rm -r /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/lib/html \
  && rm -r /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/lib/latex \
  && rm -r /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/lib/llvm-* \
  && rm -r /root/.ghcup/ghc/$GHC_VERSION/share \
  && rm -r /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/lib/x86_64-linux-ghc-$GHC_VERSION/ghc-$GHC_VERSION \
  && rm -r /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/lib/x86_64-linux-ghc-$GHC_VERSION/Cabal-* \
  && find /root/.ghcup/ -name '*.p_hi' -delete \
  && find /root/.ghcup/ -name '*_p.a' -delete \
  && find /root/.ghcup/ -name '*_debug.a' -delete \
  && find /root/.ghcup/ -name '*._debug_p.a' -delete

FROM docker.io/library/alpine:3.20

COPY --from=build /root/.ghcup /root/.ghcup

ENV PATH="/root/.ghcup/bin:$PATH"

# Install system dependencies for cabal (curl), ghc (gmp and ncurses).
RUN apk upgrade --no-cache \
  && apk add --no-cache \
    curl \
    gcc \
    gmp-dev \
    ncurses-dev \
    zlib-dev \
    zlib-static

WORKDIR /spex

COPY spex.cabal cabal.project cabal.project.freeze example/*/*.cabal .

# https://docs.docker.com/reference/dockerfile/#run---mount
RUN --mount=type=cache,target=/root/.local/state/cabal/store \
    --mount=type=cache,target=/root/.cache/cabal/packages \
    --mount=type=cache,target=dist-newstyle \
  cabal configure \
    --enable-executable-static \
    --disable-profiling \
    --disable-library-for-ghci \
    --enable-library-stripping \
    --enable-executable-stripping \
    --enable-tests \
    --enable-benchmarks \
    --disable-documentation \
  && cabal update \
  && cabal build all --only-dependencies

COPY . .

# We copied example/*/*.cabal into the working directory to build all
# dependencies, but now we have all those cabal files there, in addition to
# where they originally were inside the examples folder, so we have to remove
# them.
RUN --mount=type=cache,target=/root/.local/state/cabal/store \
    --mount=type=cache,target=/root/.cache/cabal/packages \
    --mount=type=cache,target=dist-newstyle \
  find . -maxdepth 1 \( -name '*.cabal' -a ! -name spex.cabal \) -delete \
  && cabal build lib:spex lib:petstore \
  && cabal test all

ENTRYPOINT [ "/bin/sh" ]
