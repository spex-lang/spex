all: bin/spex

bin/spex: build-image
	mkdir -p "cache/cabal-cache-musl" \
		 "cache/cabal-store-musl" \
		 "cache/dist-newstyle-musl"
	docker run \
		-v "${PWD}/..":/mnt/:ro \
		-v "${PWD}/cache/cabal-cache-musl":/root/.cache/cabal \
		-v "${PWD}/cache/cabal-store-musl":/root/.local/state/cabal/store \
		-v "${PWD}/cache/dist-newstyle-musl":/mnt/dist-newstyle \
		muslghc
	mkdir -p bin
	cp "$(shell cabal list-bin spex)" bin

build-image: Dockerfile
	docker build --tag muslghc .

bin/spex.upx: bin/spex
	upx --best -q bin/spex -o bin/spex.upx

clean:
	rm bin/spex

.PHONY: all build-image clean compress-binary
