ARG GHC_VERSION=9.6.6
ARG CABAL_VERSION=3.12.1.0

FROM docker.io/library/alpine:3.20.3 AS build

ARG GHC_VERSION
ARG CABAL_VERSION

LABEL org.opencontainers.image.source=https://github.com/spex-lang/spex
LABEL org.opencontainers.image.description="GHC musl"
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

FROM docker.io/library/alpine:3.20.3

ARG GHC_VERSION
ARG CABAL_VERSION

# Copy in the bare minimum possible (I think) from .ghcup (which is massive).
COPY --from=build \
  /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/lib/x86_64-linux-ghc-$GHC_VERSION/*.so \
  /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/lib/x86_64-linux-ghc-$GHC_VERSION/
COPY --from=build \
  /root/.ghcup/bin/cabal /root/.ghcup/bin/cabal
COPY --from=build \
  /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/bin/ \
  /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/bin/
COPY --from=build \
  /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/lib/settings \
  /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/lib/settings
COPY --from=build \
  /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/lib/package.conf.d \
  /root/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/lib/package.conf.d

# Install system dependencies for cabal (curl), ghc (gmp and ncurses) and spex
# (zlib).
RUN apk upgrade --no-cache \
  && apk add --no-cache \
    curl \
    gmp-dev \
    libffi-dev \
    ncurses-dev \
    zlib-dev 

ENV PATH="/root/.ghcup/bin:$PATH"

COPY --chmod=0755 entrypoint.sh /entrypoint.sh

ENTRYPOINT [ "/entrypoint.sh" ]
