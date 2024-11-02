OS := $(shell uname -s | tr '[:upper:]' '[:lower:]')
PLATFORM := $(shell uname -m)-$(OS)
CABAL_VERSION := $(shell awk '/^version:/ { print $$2 }' spex.cabal)
RELEASED_VERSION := $(shell gh release list --limit 1 \
			--exclude-drafts --exclude-pre-releases \
			--json tagName \
			--jq '.[].tagName // "unreleased" | sub("^v"; "") ')
GITHUB_ACTIONS ?= false

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

# The find command on MacOS doesn't have a -executable flag.
ifeq ($(OS),darwin)
FIND_EXECUTABLE := -perm +0111
else
FIND_EXECUTABLE := -executable
endif

all: build-deps build test bump install release

dist-newstyle/cache/plan.json: cabal.project cabal.project.freeze spex.cabal
	cabal configure \
		--disable-profiling \
		--disable-library-for-ghci \
		--enable-library-stripping \
		--enable-executable-stripping \
		--enable-tests \
		--enable-benchmarks \
		--disable-documentation
	cabal update
	# Generate dist-newstyle/cache/plan.json which can be used as cache key.
	cabal build all --dry-run

build-deps: dist-newstyle/cache/plan.json
	cabal build all --only-dependencies

build: 
	cabal build all

test: 
	cabal test all
	cabal check

bump: 
	@echo "CABAL_VERSION=$(CABAL_VERSION)"
	@echo "RELEASED_VERSION=$(RELEASED_VERSION)"
        ifdef CABAL_VERSION
        ifdef RELEASED_VERSION
        ifneq ($(CABAL_VERSION),$(RELEASED_VERSION))
		@echo "New version!"
		echo "new-version=$(CABAL_VERSION)" >> $(GITHUB_OUTPUT)
        endif
        endif
        endif

install:
	@echo "NEW_VERSION=$(NEW_VERSION)"
	@echo "PLATFORM=$(PLATFORM)"
	@echo "GITHUB_ACTIONS=$(GITHUB_ACTIONS)"
	@echo "SPEX_BIN=$(SPEX_BIN)"
	mkdir -p $(SPEX_BIN)
        ifdef NEW_VERSION
		find ./dist-newstyle -name 'spex*' -type f $(FIND_EXECUTABLE) -exec sh -c ' \
			strip {} \
			&& cp {} $(SPEX_BIN)/$$(basename {})-$(NEW_VERSION)-$(PLATFORM)' \; 
        else
		@echo "No new version to install..."
        endif

release:
	@echo "NEW_VERSION=$(NEW_VERSION)"
	@echo "GITHUB_ACTIONS=$(GITHUB_ACTIONS)"
	@echo "SPEX_BIN=$(SPEX_BIN)"
  ifeq ($(GITHUB_ACTIONS),true)
	upx -q $(SPEX_BIN)/*
	gh release create --draft --notes-file=CHANGELOG.md \
		"v$(NEW_VERSION)" $(SPEX_BIN)/*
  else
	@echo Running locally, skipping automatic release...
	@echo 
	@echo If you really want to make a release, manually run:
	@echo 
	@echo "  upx -q $(SPEX_BIN)/spex"
	@echo "  gh release create --draft --notes-file=CHANGELOG.md \
		"v$(NEW_VERSION)" $(SPEX_BIN)/spex"
	@echo 
	@echo You might want to add more binaries. One of the reasons for releasing 
	@echo "being disabled locally, is because it's difficult to tell programatically"
	@echo exactly what binaries to include in the release. See the following issue:
	@echo https://github.com/haskell/cabal/issues/9732 for more infomation.
  endif

clean:

.PHONY: all build-deps build test bump install release clean

# Make make fail if the shell commands fail.
.SHELLFLAGS = -ec
