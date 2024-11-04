OS := $(shell uname -s | tr '[:upper:]' '[:lower:]')
PLATFORM := $(shell uname -m)-$(OS)
CABAL_VERSION := $(shell awk '/^version:/ { print $$2 }' spex.cabal)
RELEASED_VERSION := $(shell gh release list --limit 1 \
			--exclude-drafts --exclude-pre-releases \
			--json tagName \
			--jq '.[].tagName // "unreleased" | sub("^v"; "") ')
GITHUB_ACTIONS ?= false

SPEX_GIT_COMMIT ?= $(shell git rev-parse HEAD)

# This default file is used for simulating GitHub actions outputs locally:
# https://docs.github.com/en/actions/writing-workflows/choosing-what-your-workflow-does/passing-information-between-jobs
GITHUB_OUTPUT ?= "$(TEMPDIR)/spex_github_output"

ifeq ($(GITHUB_ACTIONS),true)
SPEX_BIN := "bin"
else
SPEX_BIN := $(or $(XDG_BIN_HOME),$(HOME)/.local/bin)
NEW_VERSION = "$(shell awk -F '=' '/^new-version/ \
	{ gsub(/v/, "", $$2); print $$2 }' \
	$(GITHUB_OUTPUT))"
endif

ifeq ($(OS),linux)
	CABAL := docker run --rm --entrypoint=cabal \
			--volume $(PWD):/mnt \
			--volume $(HOME)/.cache/cabal:/root/.cache/cabal \
			--volume $(HOME)/.cabal/store:/root/.local/state/cabal/store \
			--volume $(PWD)/dist-newstyle:/mnt/dist-newstyle \
			--env SPEX_GIT_COMMIT=$(SPEX_GIT_COMMIT) \
			ghcr.io/spex-lang/spex-build:latest
	ENABLE_STATIC := --enable-executable-static

else
	CABAL := cabal
	ENABLE_STATIC := ""
endif

all: build-deps build test bump install release

dist-newstyle/cache/plan.json: cabal.project cabal.project.freeze spex.cabal
ifeq ($(OS),linux)
	mkdir -p $(HOME)/.cache/cabal/packages
	mkdir -p $(HOME)/.cabal/store
endif
	$(CABAL) configure \
		$(ENABLE_STATIC) \
		--disable-profiling \
		--disable-library-for-ghci \
		--enable-library-stripping \
		--enable-executable-stripping \
		--enable-tests \
		--enable-benchmarks \
		--disable-documentation
	$(CABAL) update
	# Generate dist-newstyle/cache/plan.json which can be used as cache key.
	$(CABAL) build all --dry-run

build-deps: dist-newstyle/cache/plan.json
	$(CABAL) build all --only-dependencies

build: 
	$(CABAL) update
	$(CABAL) build all

test: 
	$(CABAL) test all
	$(CABAL) check

bump: 
	@echo "CABAL_VERSION=$(CABAL_VERSION)"
	@echo "RELEASED_VERSION=$(RELEASED_VERSION)"
	@echo "OS=$(OS)"
        ifdef CABAL_VERSION
        ifdef RELEASED_VERSION
        ifneq ($(CABAL_VERSION),$(RELEASED_VERSION))
		@echo "New version!"
		# https://github.com/actions/runner/issues/2224
                ifeq ($(findstring msys_nt,$(OS)),msys_nt)
			echo "new-version=$(CABAL_VERSION)" >> $Env:GITHUB_OUTPUT
                else
			echo "new-version=$(CABAL_VERSION)" >> $(GITHUB_OUTPUT)
                endif
        endif
        endif
        endif

install:
	$(CABAL) install all --installdir=$(SPEX_BIN) \
		--install-method=copy --overwrite-policy=always

release:
	@echo "NEW_VERSION=$(NEW_VERSION)"
	@echo "GITHUB_ACTIONS=$(GITHUB_ACTIONS)"
	@echo "SPEX_BIN=$(SPEX_BIN)"
  ifeq ($(GITHUB_ACTIONS),true)
	ls -R $(SPEX_BIN)
	for dir in $$(ls $(SPEX_BIN)); do \
		for bin in $$(ls $(SPEX_BIN)/$$dir); do \
			mv $(SPEX_BIN)/$$dir/$$bin \
			   $(SPEX_BIN)/$$(basename $$bin)-$(NEW_VERSION)-$$(basename $$dir); \
			chmod 755 $(SPEX_BIN)/$$(basename $$bin)-$(NEW_VERSION)-$$(basename $$dir); \
		done \
	done
	upx -q $(SPEX_BIN)/spex-*
	gh release create --draft --notes-file=CHANGELOG.md \
		"v$(NEW_VERSION)" $(SPEX_BIN)/spex-*
  else
	@echo Running locally, skipping automatic release...
  endif

clean:

pull-image:
	docker pull ghcr.io/spex-lang/spex-build:latest

build-image: Dockerfile
	docker build --tag ghcr.io/spex-lang/spex-build:latest .

push-image:
	docker push ghcr.io/spex-lang/spex-build:latest

.PHONY: all build-deps build test bump install release clean build-image

# Make make fail if the shell commands fail.
.SHELLFLAGS = -ec
