# This Makefile is supposed to work on Linux, MacOS and Windows (with WSL and
# GNU make). On Linux it should build static binaries using an Alpine
# container. All this should work both on GitHub actions CI and when run
# locally.

# In order to do different things on different operating systems (OSes), we
# first need to find out which OS we are running on. To avoid having to
# remember the casing of the OS, we lowercase the OS.
OS := $(shell uname -s | tr '[:upper:]' '[:lower:]')

# This variable is set to true on GitHub's CI. If it's not set, we default to
# false. This let's us distinguish between local and CI runs.
GITHUB_ACTIONS ?= false

# Used for the --version flag to print at which commit the binary was build.
# This gets passed in from CI, or default to the below in case of local runs.
SPEX_GIT_COMMIT ?= $(shell git rev-parse HEAD)

# Where binaries will be "installed". This isn't in PATH because we want to
# compress and upload these, and therefore it's easier if they stay in a
# separate directory. If we actually wanna install them into PATH, see the
# `spexup` target.
SPEX_BIN := dist-newstyle/bin

# This is where `cabal` caches some of its data, it's important mount these
# directories when building inside an image, otherwise work will need to be
# redone between the steps. We also want to cache these directories on CI. 
#
# The packages cache is massive though as it contains 00-index.tar,
# 00-index.tar.gz which are two copies of all packages ever published on
# Hackage... We should try to delete those before caching the directory on CI.
CABAL_PACKAGES_CACHE := $(HOME)/.cabal/packages
CABAL_STORE := $(HOME)/.cabal/store

ifeq ($(OS),linux)
	CABAL := docker run --rm --entrypoint=cabal \
			--volume $(PWD):/mnt \
			--volume $(CABAL_PACKAGES_CACHE):/root/.cache/cabal/packages \
			--volume $(CABAL_STORE):/root/.local/state/cabal/store \
			--env SPEX_GIT_COMMIT=$(SPEX_GIT_COMMIT) \
			ghcr.io/spex-lang/spex-build:latest
	ENABLE_STATIC := --enable-executable-static
else
	CABAL := cabal
	ENABLE_STATIC := 
endif

# XXX: Move to cabal.project file?
CABAL_CONFIGURE_FLAGS ?= $(ENABLE_STATIC) --disable-profiling \
	--disable-library-for-ghci --enable-library-stripping \
	--enable-executable-stripping --enable-tests --enable-benchmarks \
	--disable-documentation

# Since all our dependencies are pinned in the cabal.project.freeze file, we
# can also pin `cabal update` using "index-state" to avoid fetching metadata
# about new packages
# (see https://cabal.readthedocs.io/en/stable/cabal-project-description-file.html#cfg-field-index-state).
#
# However due to `cabal update` not respecting the index state set in
# cabal.project (see https://github.com/haskell/cabal/issues/9039) we have to
# manually pass the index-state when calling `cabal update`. In order to be
# able to do so we first need to extract the index-state. For more see:
# https://stackoverflow.com/a/48758099 .
INDEX_STATE := $(shell awk '/index-state:/ { print $$2","$$3 }' cabal.project)

# Make make fail if a shell command fails.
.SHELLFLAGS = -ec

# ----------------------------------------------------------------------

.PHONY: all build-deps build test bump install compress release rmtempdir

all: build-deps build test bump install compress release rmtempdir

dist-newstyle/cache/plan.json: cabal.project cabal.project.freeze spex.cabal
	# These directories need to exist or the container volume mount will
	# fail.
	mkdir -p $(CABAL_PACKAGES_CACHE)
	mkdir -p $(CABAL_STORE)
	# In case we want to install the binaries into dist-newstyle/bin, then
	# we better create that directory now, otherwise `$(CABAL)` will create
	# it and if it runs inside docker then it will create it with root
	# permissions (which would cause later calls to mkdir to fail due to
	# permissions).
	mkdir -p $(SPEX_BIN)
	$(CABAL) configure $(CABAL_CONFIGURE_FLAGS)
	$(CABAL) update $(INDEX_STATE)
	# Generate dist-newstyle/cache/plan.json which can be used as cache key
	# on CI.
	$(CABAL) build all --dry-run

build-deps: dist-newstyle/cache/plan.json
	$(CABAL) build all --only-dependencies

build: 
	# XXX: why do we need a second update? Maybe because we are not caching
	# ~/.cabal/packages between calls?
	$(CABAL) update $(INDEX_STATE)
	$(CABAL) build all

test: 
	$(CABAL) test all
	$(CABAL) check

# Avoid using `cabal install`, because:
# 
#   1. It doesn't work from within a container (as it will merely install
#      inside the container);
#   2. Due to a bug cabal install sometimes recompiles the binary
#      (https://github.com/haskell/cabal/issues/6919).
#
# Instead use `find` to find the binaries inside the dist-newstyle directory.
# The `find` utility works slightly different on darwin/macOS with regards to
# how we can filter out executables only.
ifeq ($(OS),darwin)
  FIND_EXECUTABLE := -perm +0111
else
  FIND_EXECUTABLE := -executable
endif

install:
	find dist-newstyle/ -not -path "$(SPEX_BIN)/*" -name 'spex*' -type f \
		$(FIND_EXECUTABLE) -exec cp {} $(SPEX_BIN)/ \;

# NOTE: upx doesn't currently work on macOS, the error I get on CI is:
#   "CantPackException: macOS is currently not supported"
# There's a bit more information in this closed ticket:
#   https://github.com/upx/upx/issues/777#issuecomment-1909535310
# So instead of compressing, we simply strip on macOS.
compress:
  ifeq ($(OS),darwin)
	find $(SPEX_BIN) -name 'spex*' -type f \
		$(FIND_EXECUTABLE) -exec strip {} \;
  else
	upx -q $(SPEX_BIN)/spex*
  endif

release:
	ls -R $(SPEX_BIN)
  # On GitHub CI each OS would build its own binaries and upload them, then
  # another job would download the binaries and create the release. When the
  # binaries get downloaded for the different OSes they get put in separate
  # folders, the following roughly simulates that locally.
  ifeq ($(GITHUB_ACTIONS),false)
	mkdir -p $(SPEX_BIN)/$(OS)
	mv $(SPEX_BIN)/spex* $(SPEX_BIN)/$(OS)
	ls -R $(SPEX_BIN)
  endif
	for dir in $$(ls $(SPEX_BIN)); do \
		for bin in $$(ls $(SPEX_BIN)/$$dir); do \
			suffix=""; \
			case $$bin in \
			  *".exe") suffix=".exe" ;; \
			  *) ;; \
			esac; \
			mv $(SPEX_BIN)/$$dir/$$bin \
			   $(SPEX_BIN)/$$(basename $$bin $$suffix)-$(NEW_VERSION)-$$(basename $$dir)$$suffix; \
		done \
	done
  ifeq ($(GITHUB_ACTIONS),true)
	gh release create --prerelease --notes-file=CHANGELOG.md \
		"v$(NEW_VERSION)" $(SPEX_BIN)/spex*
  else
	@echo Running locally, skipping automatic release...
  endif

rmtempdir:
	rm "$(SPEX_TEMP)/spex_github_output"
	rmdir "$(SPEX_TEMP)"

# ----------------------------------------------------------------------

# In order to automatically create releases we compare the version field in the
# spex.cabal file with the latest release on GitHub.

# XXX: Add --exclude-pre-releases \ later once we made a proper release...
CABAL_VERSION := $(shell awk '/^version:/ { print $$2 }' spex.cabal)
RELEASED_VERSION := $(shell gh release list --limit 1 \
			--exclude-drafts \
			--json tagName \
			--jq '.[].tagName // "unreleased" | sub("^v"; "") ')

# This variable is set by GitHub's CI and allows us to pass information between
# jobs, see: 
#   https://docs.github.com/en/actions/writing-workflows/choosing-what-your-workflow-does/passing-information-between-jobs
# The information we want to pass is if a new version is pushed, see the `bump`
# target. In order to make this work locally we use a temporary file and
# effectively simulate the CI behaviour.
SPEX_TEMP := $(shell mktemp -d)
GITHUB_OUTPUT ?= "$(SPEX_TEMP)/spex_github_output"

ifeq ($(GITHUB_ACTIONS),false)
  NEW_VERSION = "$(shell awk -F '=' '/^new-version/ \
	{ gsub(/v/, "", $$2); print $$2 }' \
	$(GITHUB_OUTPUT))"
endif

# I had trouble setting output variables on GitHub Actions when using the
# default make shell on Windows runners, see:
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

# ----------------------------------------------------------------------
# Smoke test

.PHONY: spexup smoke

spexup:
	export GH_TOKEN=$(GH_TOKEN); \
	curl --proto '=https' --tlsv1.2 -sSf \
		https://raw.githubusercontent.com/spex-lang/spexup/refs/heads/main/spexup \
	| sh

smoke:
	export PATH="$$PATH:/usr/local/bin:$$HOME/.local/bin" && \
	spex --version | grep "v${CABAL_VERSION} ${SPEX_GIT_COMMIT}" 

# ----------------------------------------------------------------------

.PHONY: clean

clean:
	rm -rf dist-newstyle

distclean: clean
	rm cabal.project.local*

# ----------------------------------------------------------------------
# Pull, build and push images

# These references will be passed in by GitHub CI and are used to check if the
# Dockerfile has changed. When run locally we default back on checking if the
# Dockerfile has changed between HEAD and origin/main.
GITHUB_EVENT_AFTER  =? HEAD
GITHUB_EVENT_BEFORE =? origin/main

DOCKERFILE_CHANGED := $(shell \
	git diff --name-only "$(GITHUB_EVENT_AFTER)" "$(GITHUB_EVENT_BEFORE)" \
	| grep Dockerfile && echo true || echo false)

.PHONY: pull-image build-image push-image

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

.PHONY: build-app-image cr-login

build-app-image: Dockerfile.app
	docker build \
		--volume $(CABAL_PACKAGES_CACHE):/root/.cache/cabal/packages \
		--volume $(CABAL_STORE):/root/.local/state/cabal/store \
                --env=SPEX_GIT_COMMIT=$(SPEX_GIT_COMMIT) \
		--tag ghcr.io/spex-lang/spex:latest \
		--file Dockerfile.app

cr-login:
	# This needs to be a personal access tokens (classic) with "repo,
	# write:packages" permissions.
	@echo $(CR_PAT) | \
		docker login ghcr.io --username spex-lang --password-stdin

# ----------------------------------------------------------------------
# Formatting

.PHONY: format

format:
	find src/ -name '*.hs' -type f -exec fourmolu -q -i {} \;

