GHC_VERSION:=9.6.6

all: bin/spex bin/spex-demo-petstore

# We can't use cabal list-bin here, because it thinks the binary is in
# ../dist-newstyle rather than cache/dist-newstyle-musl.
bin/spex: static-build
	@mkdir -p bin
	cp "$(shell find cache/dist-newstyle-musl \
		-name spex -type f -executable)" bin
	
bin/spex-demo-petstore: static-build
	@mkdir -p bin
	cp "$(shell find cache/dist-newstyle-musl \
		-name spex-demo-petstore -type f -executable)" bin

static-build: build-image
	mkdir -p cache/cabal-cache-musl \
		 cache/cabal-store-musl \
		 cache/dist-newstyle-musl 
	docker run \
		-v "${PWD}/..":/mnt/:ro \
		-v "${PWD}/cache/cabal-cache-musl":/root/.cache/cabal/packages \
		-v "${PWD}/cache/cabal-store-musl":/root/.local/state/cabal/store \
		-v "${PWD}/cache/dist-newstyle-musl":/mnt/dist-newstyle \
		ghcr.io/spex-lang/static-build:$(GHC_VERSION) \
		update hackage.haskell.org,@1729577343 \
	docker run \
		-v "${PWD}/..":/mnt/:ro \
		-v "${PWD}/cache/cabal-cache-musl":/root/.cache/cabal \
		-v "${PWD}/cache/cabal-store-musl":/root/.local/state/cabal/store \
		-v "${PWD}/cache/dist-newstyle-musl":/mnt/dist-newstyle \
		ghcr.io/spex-lang/static-build:$(GHC_VERSION) \
		build exe:spex exe:spex-demo-petstore --enable-executable-static --project-dir=/mnt

build-image: Dockerfile
	docker build --tag ghcr.io/spex-lang/static-build:$(GHC_VERSION) .

push-image: build-image
	@echo ${CR_PAT} | \
		docker login ghrc.io --username spex-lang --password-stdin
	docker push ghcr.io/spex-lang/static-build:$(GHC_VERSION)

bin/spex.upx: bin/spex
	upx --best -q bin/spex -o bin/spex.upx

clean:
	rm -f bin/spex
	rm -f bin/spex.upx
	rm -f bin/spex-demo-petstore

distclean: clean
	rm -rf cache

.PHONY: all static-build build-image push-image clean distclean
