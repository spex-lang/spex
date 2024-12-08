FROM scratch

LABEL org.opencontainers.image.source=https://github.com/spex-lang/spex
LABEL org.opencontainers.image.description="The Spex specification language and toolkit"
LABEL org.opencontainers.image.licenses=BSD-2-Clause

ARG SPEX_BIN
ARG NEW_VERSION

COPY --chmod 755 $SPEX_BIN/spex-$NEW_VERSION-x86_64-linux /spex
COPY --chmod 755 $SPEX_BIN/spex-demo-petstore-$NEW_VERSION-x86_64-linux /spex-demo-petstore

ENV PATH="/:$PATH"

ENTRYPOINT [ "/spex" ]
