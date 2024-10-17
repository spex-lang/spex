# Inspired by https://entropicthoughts.com/deploying-single-binary-haskell-web-app

FROM docker.io/library/alpine:3.20.3

RUN apk update \
  && apk add --no-cache \
       autoconf automake bash binutils-gold curl dpkg fakeroot file \
       findutils g++ gcc git make perl shadow tar xz \
  && apk add --no-cache \
       brotli brotli-static \
       bzip2 bzip2-dev bzip2-static \
       curl libcurl curl-static \
       freetype freetype-dev freetype-static \
       gmp-dev \
       libffi libffi-dev \
       libpng libpng-static \
       ncurses-dev ncurses-static \
       openssl-dev openssl-libs-static \
       pcre pcre-dev \
       pcre2 pcre2-dev \
       sdl2 sdl2-dev \
       sdl2_image sdl2_image-dev \
       sdl2_mixer sdl2_mixer-dev \
       sdl2_ttf sdl2_ttf-dev \
       xz xz-dev \
       zlib zlib-dev zlib-static \
  && ln -s /usr/lib/libncursesw.so.6 /usr/lib/libtinfo.so.6

ENV GHCUP_INSTALL_BASE_PREFIX=/usr/local

RUN curl --fail --output /bin/ghcup \
        'https://downloads.haskell.org/ghcup/x86_64-linux-ghcup' \
  && chmod 0755 /bin/ghcup \
  && ghcup upgrade --target /bin/ghcup \
  && ghcup install cabal --set \
  && /usr/local/.ghcup/bin/cabal update

ENV PATH="/usr/local/.ghcup/bin:$PATH"

RUN ghcup install cabal 3.12.1.0
RUN ghcup install ghc 9.6.6 --set

ENTRYPOINT [ "/mnt/release/entrypoint.sh" ]
