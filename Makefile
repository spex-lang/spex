# This Makefile is supposed to work on Linux, MacOS and Windows (with WSL and
# GNU make). On Linux it should build static binaries using an Alpine
# container. All this should work both on GitHub actions CI and when run
# locally.

OS := $(shell uname -s | tr '[:upper:]' '[:lower:]')
PLATFORM := $(shell uname -m)-$(OS)
CABAL_VERSION := $(shell awk '/^version:/ { print $$2 }' spex.cabal)
RELEASED_VERSION := $(shell gh release list --limit 1 \
			--exclude-drafts --exclude-pre-releases \
			--json tagName \
			--jq '.[].tagName // "unreleased" | sub("^v"; "") ')
# cabal update doesn't respect index state in cabal.project, see:
#   https://github.com/haskell/cabal/issues/9039
INDEX_STATE := $(shell awk '/index-state:/ { print $$2","$$3 }' cabal.project)

# This variable is set to true on GitHub's CI.
GITHUB_ACTIONS ?= false

SPEX_GIT_COMMIT ?= $(shell git rev-parse HEAD)

GITHUB_EVENT_AFTER =? HEAD
GITHUB_EVENT_BEFORE =? origin/main

DOCKERFILE_CHANGED := $(shell git diff --name-only "$(GITHUB_EVENT_AFTER)" "$(GITHUB_EVENT_BEFORE)" \
			| grep Dockerfile && echo true || echo false)

# This variable is set by GitHub's CI and allows us to pass information between
# jobs, see: 
#   https://docs.github.com/en/actions/writing-workflows/choosing-what-your-workflow-does/passing-information-between-jobs
# The information we want to pass is if a new version is pushed, see the `bump`
# target. In order to make this work locally we use a temporary file and
# effectively simulate the CI behaviour.
SPEX_TEMP := $(shell mktemp -d)
GITHUB_OUTPUT ?= "$(SPEX_TEMP)/spex_github_output"

# Make make fail if the shell commands fail.
.SHELLFLAGS = -ec

ifeq ($(GITHUB_ACTIONS),true)
SPEX_BIN := bin
else
SPEX_BIN := $(or $(XDG_BIN_HOME),$(HOME)/.local/bin)
NEW_VERSION = "$(shell awk -F '=' '/^new-version/ \
	{ gsub(/v/, "", $$2); print $$2 }' \
	$(GITHUB_OUTPUT))"
endif

ifeq ($(OS),linux)
  ifeq ($(GITHUB_ACTIONS),true)
	CABAL := docker run --rm --entrypoint=cabal \
			--volume $(PWD):/mnt \
			--volume $(HOME)/.cache/cabal/packages:/root/.cache/cabal/packages \
			--volume $(HOME)/.cabal/store:/root/.local/state/cabal/store \
			--env SPEX_GIT_COMMIT=$(SPEX_GIT_COMMIT) \
			ghcr.io/spex-lang/spex-build:latest
	ENABLE_STATIC := --enable-executable-static
  else
	CABAL := docker run --rm --entrypoint=cabal \
			--volume $(PWD):/mnt \
			--volume $(PWD)/.container-cache/cabal/packages:/root/.cache/cabal/packages \
			--volume $(PWD)/.container-cache/cabal/store:/root/.local/state/cabal/store \
			--env SPEX_GIT_COMMIT=$(SPEX_GIT_COMMIT) \
			ghcr.io/spex-lang/spex-build:latest
	ENABLE_STATIC := --enable-executable-static
  endif
else
	CABAL := cabal
	ENABLE_STATIC := 
endif

all: build-deps build test bump install compress release rmtempdir

# XXX: doesn't configure petstore...
dist-newstyle/cache/plan.json: cabal.project cabal.project.freeze spex.cabal
ifeq ($(OS),linux)
  ifeq ($(GITHUB_ACTIONS),true)
	mkdir -p $(PWD)/dist-newstyle
	mkdir -p $(HOME)/.cache/cabal/packages
	mkdir -p $(HOME)/.cabal/store
  else
	mkdir -p $(PWD)/dist-newstyle
	mkdir -p $(PWD)/.container-cache/cabal/packages
	mkdir -p $(PWD)/.container-cache/cabal/store
  endif
endif
	$(CABAL) configure $(ENABLE_STATIC) --disable-profiling  --disable-library-for-ghci --enable-library-stripping --enable-executable-stripping --enable-tests --enable-benchmarks --disable-documentation
	$(CABAL) update $(INDEX_STATE)
	# Generate dist-newstyle/cache/plan.json which can be used as cache key.
	$(CABAL) build all --dry-run

build-deps: dist-newstyle/cache/plan.json
	$(CABAL) build all --only-dependencies

build: 
	# XXX: shouldn't be needed?
	$(CABAL) update $(INDEX_STATE)
	$(CABAL) build all

test: 
	$(CABAL) test all
	$(CABAL) check

# I had trouble setting output variables on GitHub Actions when using the
# default shell on Windows runners, see:
#   https://github.com/actions/runner/issues/2224
#   https://stackoverflow.com/questions/74443940/value-not-set-using-github-output
# It seems to work with pwsh though. The layers of WSL on Windows runner, shell
# on GitHub Actions, and shell in GNU make make things complicated.
ifeq ($(findstring mingw64_nt,$(OS)),mingw64_nt) 
bump: SHELL := pwsh.exe
bump: .SHELLFLAGS := -Command
endif
bump: 
	@echo "CABAL_VERSION=$(CABAL_VERSION)"
	@echo "RELEASED_VERSION=$(RELEASED_VERSION)"
	@echo "OS=$(OS)"
	@echo "SHELL=$(SHELL)"
	@echo "GITHUB_OUTPUT=$(GITHUB_OUTPUT)"
        ifdef CABAL_VERSION
        ifdef RELEASED_VERSION
        ifneq ($(CABAL_VERSION),$(RELEASED_VERSION))
		@echo "New version!"
		echo "new-version=$(CABAL_VERSION)" >> $(GITHUB_OUTPUT)
        endif
        endif
        endif

install:
  ifeq ($(OS),linux)
	# Running `cabal install` inside a container will it inside the
	# container, which isn't what we want. Instead find the binary inside
	# dist-newstyle, which is shared with the host via a volume mount, and
	# copy it from there to the right place.
	mkdir -p $(SPEX_BIN)/
	find dist-newstyle/ -name 'spex*' -type f -executable -exec cp {} $(SPEX_BIN)/ \;
  else
	$(CABAL) install all --installdir=$(SPEX_BIN) --install-method=copy --overwrite-policy=always
  endif

compress:
	upx -q $(SPEX_BIN)/spex*


release:
	@echo "NEW_VERSION=$(NEW_VERSION)"
	@echo "GITHUB_ACTIONS=$(GITHUB_ACTIONS)"
	@echo "SPEX_BIN=$(SPEX_BIN)"
  ifeq ($(GITHUB_ACTIONS),true)
	ls -R $(SPEX_BIN)
	for dir in $$(ls $(SPEX_BIN)); do \
		for bin in $$(ls $(SPEX_BIN)/$$dir); do \
			case $$bin in \
			  *".exe") \
				mv $(SPEX_BIN)/$$dir/$$bin \
				   $(SPEX_BIN)/$$(basename $$bin .exe)-$(NEW_VERSION)-$$(basename $$dir).exe; \
				chmod 755 $(SPEX_BIN)/$$(basename $$bin .exe)-$(NEW_VERSION)-$$(basename $$dir).exe ;; \
			  *) \
				mv $(SPEX_BIN)/$$dir/$$bin \
				   $(SPEX_BIN)/$$(basename $$bin)-$(NEW_VERSION)-$$(basename $$dir); \
				chmod 755 $(SPEX_BIN)/$$(basename $$bin)-$(NEW_VERSION)-$$(basename $$dir);; \
			esac \
		done \
	done
	gh release create --draft --notes-file=CHANGELOG.md \
		"v$(NEW_VERSION)" $(SPEX_BIN)/spex*
  else
	@echo Running locally, skipping automatic release...
  endif

rmtempdir:
	rm "$(SPEX_TEMP)/spex_github_output"
	rmdir "$(SPEX_TEMP)"

spexup:
	export GH_TOKEN=$(GH_TOKEN); \
	curl --proto '=https' --tlsv1.2 -sSf \
		https://raw.githubusercontent.com/spex-lang/spexup/refs/heads/main/spexup \
	| sh

ifeq ($(OS),darwin)
smoke: SHELL := bash
endif
smoke:
	@echo "SHELL=$(SHELL)"
	ls -l $(HOME)/.local/bin/ || true
	ls -l /usr/local/bin/ || true
	export PATH="$$PATH:/usr/local/bin:$$HOME/.local/bin" && \
	echo $$PATH && \
	ldd $$(which spex) || true ; \
	otool -L $$(which spex) || true ; \
	spex --version || true ; \
	spex --version | grep "v${CABAL_VERSION} ${SPEX_GIT_COMMIT}" 
	# log show --last 10m --debug --predicate 'eventMessage contains "spex"' || true

clean:
	rm -rf dist-newstyle

distclean: clean
	rm -rf .container-cache

pull-image:
  ifeq ($(DOCKERFILE_CHANGED),false)
	  docker pull ghcr.io/spex-lang/spex-build:latest
  endif

build-image: Dockerfile
  ifeq ($(DOCKERFILE_CHANGED),true)
	  docker build --tag ghcr.io/spex-lang/spex-build:latest .
  endif

push-image:
  ifeq ($(DOCKERFILE_CHANGED),true)
	  docker push ghcr.io/spex-lang/spex-build:latest
  endif

.PHONY: all build-deps build test bump install release spexup smoke clean distclean build-image
