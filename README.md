# Spex

[![Zulip](https://img.shields.io/badge/zulip-join_chat-blue.svg)](https://spex.zulipchat.com/)
[![IRC: #spex on libera.chat](https://img.shields.io/badge/IRC-%23spex%20on%20libera.chat-blue.svg)](https://web.libera.chat/#spex)
![Build status](https://github.com/spex-lang/spex/actions/workflows/main.yaml/badge.svg)

Spex is a specification language and verifier that uses fuzzing and generative
testing techniques to check if software is faithful to some specification.

It's not a programming language in the traditional sense, meaning that the
software it checks needs to first be written in another language. Another way
to think of it, is that `spex` let's you specify *what* the software under test
should to, but not *how* it does it.

Currently it only supports specifying and testing HTTP API servers, but the
range of software that can be specified and tested will extended in the future.

> [!CAUTION]
> Spex is pre-alpha and not suitable for general use yet.

## Features

- [x] Concise specification language for HTTP API servers
  <details>
  
  <summary>Example</summary>
  
  ```
  $ cat example/petstore-basic.spex
  component PetStore where
  
  addPet : POST /pet Pet
  getPet : GET /pet/{petId : Int} -> Pet
  
  type Pet =
    { petId   : Int
    , petName : String
    }
  ```
  
  </details>

- [x] Ability to test specification against a deployment
  <details>

  <summary>Example</summary>

  ```bash
  $ spex-demo-petstore &
  $ PID_PETSTORE=$!
  $ spex example/petstore-basic.spex

  i Verifying the deployment:    http://localhost:8080
    against the specification:   example/petstore-basic.spex
  
  i Parsing the specification.
  
  i Waiting for health check to pass.
  
  i Starting to run tests.
  
  i All tests passed, here are the results:
  
    failing tests: []
    client errors: 53
    coverage:      fromList [(OpId "addPet",44),(OpId "getPet",56)]
  $ kill ${PID_PETSTORE}
  [1]+  Terminated              spex-demo-petstore
  ```

  </details>

- [x] Keep track of previously generated values and sometimes try to use them
  during generation of new tests. For example, without this ability the
  `getPet` requests would all most certainly return 404.

- [x] Ability to annotate input types with abstract and unique modalities (@ and
  !). Where an abstract type isn't generated, i.e. gets reused, and a unique type
  is always generated and never reused. Without any annotation a coin is
  flipped and the value either gets reused or generated.
  <details>

  <summary>Example</summary>

  ```diff
  $ diff -u example/petstore-basic.spex example/petstore-modal.spex
  - addPet : POST /pet Pet
  - getPet : GET /pet/{petId : Int} -> Pet
  + addPet : POST /pet !Pet
  + getPet : GET /pet/{petId : @Int} -> Pet
  $ spex example/petstore-modal.spex
  cabal run spex -- example/petstore-modal.spex

  i Verifying the deployment:    http://localhost:8080
    against the specification:   example/petstore-modal.spex
  
  i Parsing the specification.
  
  i Waiting for health check to pass.
  
  i Starting to run tests.
  
  i All tests passed, here are the results:
  
    failing tests: []
    client errors: 3
    coverage:      fromList [(OpId "addPet",51),(OpId "getPet",49)]
  ```

  Notice how many fewer 404 errors we get for `getPet` now, because of the
  abstract (`@`) annotation on `petId`.
  </details>

- [ ] Keep track of previous responses and try to use them during generation 

- [ ] Nice CLI and errors
  + https://elm-lang.org/news/compiler-errors-for-humans
  + https://hackage.haskell.org/package/diagnose-2.5.1/docs/Error-Diagnose.html
  + https://medium.com/designing-atlassian/10-design-principles-for-delightful-clis-522f363bac87
  + https://github.com/charmbracelet/bubbletea
  + https://gleam.run/

- [ ] Coverage statistics and use coverage-guidance (endpoint coverage,
  but also check coverage of the range of values from responses.)

- [ ] Shrinking

- [ ] User provided coverage and minimal test cases
  + E.g. show me all the minimal test cases that lead to each error?

- [ ] Formatting of specs
  <details>

  <summary>Example</summary>

  ```bash
  $ cat example/petstore-badly-formatted.spex
  component PetStore
    where
  
  addPet     : POST   
    /pet Pet

  getPet :GET /pet/{ petId  : 
    Int} ->
    Pet
  $ spex fmt example/petstore-badly-formatted.spex
  component PetStore where
  
  addPet : POST /pet Pet
  getPet : GET /pet/{petId : Int} -> Pet
  ```

  </details>

- [ ] Don't stop if error is found

- [ ] Only presents new errors?

## Installation

### From precompiled binary

#### Automatic

```bash
 curl --proto '=https' --tlsv1.2 -sSf \
   https://raw.githubusercontent.com/spex-lang/spexup/refs/heads/main/spexup \
 | sh
```

What does this do? It
[automates](https://github.com/spex-lang/spexup/blob/main/spexup) the manual
steps below.

#### Manual

1. Go to [releases](https://github.com/spex-lang/spex/releases);
2. Click on "Assets" for the latest release;
3. Download the binaries and put them into your PATH.

### From source

#### With Nix

Install the [Nix](https://nixos.org/download/) package manager and then do:

```bash
git clone https://github.com/spex-lang/spex.git
cd spex
nix-shell
cabal build all
cabal install spex spex-demo-petstore
```

#### Without Nix

Install [`ghcup`](https://www.haskell.org/ghcup/install/), the
[Haskell](https://www.haskell.org/) installer, and then do:

```bash
git clone https://github.com/spex-lang/spex.git
cd spex
ghcup install cabal 3.12.1.0 --set
ghcup install ghc 9.6.6 --set
cabal build all
cabal install spex spex-demo-petstore
```

## Contributing

Spex is at an early stage of development and there are many ways to help out,
here are some examples:

* Help out with issues and PRs;
* Contribute artwork under appropriate licenses (at least CC-BY-SA);
* Be helpful and nice to others on:
  - IRC in the channel [#spex](https://web.libera.chat/#spex) on `irc.libera.chat`;
  - [Zulip](https://spex.zulipchat.com/).
* Contributed to releated projects:
  - [`spexup`](https://github.com/spex-lang/spexup), the installer for the Spex
    language.
