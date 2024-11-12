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
range of software that can be specified and tested will extended in the
future.

> [!CAUTION]
> Spex is pre-alpha and not suitable for general use yet. Please don't share
> just yet, I'll announce it properly soon.

## Motivation

Few people write specifications for their software these days. The reason for
this clear: there are few benefits from doing so, especially when taking into
account the risk of the specification and the real system drifting out of sync
(i.e. either the system or the specification is changed, but we forget to
update the other).

Spex tries to address this shortcoming of specifications by in addition to
being a specification language, it's also a verifier that checks if some system
respects the specification -- thereby always ensuring that the two are in sync.

In the process of testing the real system against the specification is will
also produce minimal test cases for potential problems it notices along the way:

  - Non-2xx responses;
  - JSON response decoding and type issues;
  - Non-reachable APIs.

(We'll look at examples of how exactly this gets reported in the next section on
features.)

In the future we'd like to derive more useful functionality from
specifications, including:

  - Ability to import and export OpenAPI/Swagger, Protobuf, etc. Think of how
    Pandoc can covert between text formats, perhaps we can do the same between
    specifications;
  - Generate a prototype implementation from a specification, so that you can
    demo your idea or hand of a working server HTTP API to the frontend team
    before the actual backend is done (without risking that there will be a
    mismatch in the end, since the real backend is tested against the same
    specification as the prototype is derived from);
  - A REPL, which allows you to explore a system using a specification. Tab
    completion is provided for the API and random payload data is generated on
    the fly;
  - A time traveling debugger which enables you to step forwards and backwards
    through a sequence of API calls, in order to explore how the system evolves
    over time.
  - Lua templating (again similar to Pandoc) which enables code generation from
    specifications or the minimal test cases that the verifer produces;
  - The ability to refine types, e.g. `{ petId : Int | petId > 0 }` and be able
    to generate validation logic from these;
  - Generate diagrams for a better overview of how components are connected.

With this future functionality we hope to get to the point where there's a
clear benefit to writing specifications!

## Features

Here are the currently supported features. Click on the "▸ Example" buttons for
more details about how to use them.

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
  $ spex verify example/petstore-basic.spex

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
  $ spex verify example/petstore-modal.spex

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

- [x] Keep track of previous responses and try to use them during generation 

  <details>

  <summary>Example</summary>

  Imagine we got:
  ```
  addPet : POST /pet Pet
  getPet : GET /pet/{petId : Int} -> Pet

  type Pet =
    { petId   : Int
    , petName : String 
    }
  ```
  and `addPet` is implemented such that it throws an error if we try to add the
  exact same pet twice. Finding this error without reusing responses during
  generation is difficult, because we'd need to randomly generate the same
  `petId : Int` and petname : String` twice. 

  If we reuse inputs and reponses on the other hand, then it's easy to find it.
  Here's one scenario which would find the error:

    1. We generate a random `Pet` for `addPet`;
    2. A `getPet` operation gets generated that reuses the `petId` from step 1;
    3. The response `Pet` from step 2 gets reused in a subsequent `addPet`,
       casuing the error.

  </details>

- [x] Nice command-line interface and errors for humans

  <details>

  <summary>Example</summary>

  ```
  $ cat example/petstore-bad-scope.spex
  component PetStore where

  addPet : POST /pet Pet
  getPet : GET /pet/{petId : Int} -> Pet

  $ spex verify example/petstore-bad-scope.spex
  i Verifying the deployment:    http://localhost:8080
    against the specification:   example/petstore-bad-scope.spex

  i Checking the specification.

  Error: Scope error, the type Pet isn't defined.

     ┌─ example/petstore-bad-scope.spex:2:19
     │
   2 │ addPet : POST /pet Pet
     │                    ^^^

  Either define the type or mark it as abstract, in case it shouldn't be
  generated.
  ```

  </details>

- [x] Test case minimisation aka shrinking
  <details>

  <summary>
  Example
  </summary>

  ```
  $ spex verify example/petstore-modal.spex --seed -2917004710203612904

  i Verifying the deployment:    http://localhost:8080
    against the specification:   example/petstore-modal.spex

  i Checking the specification.

  i Waiting for health check to pass.

  i Starting to run tests.

  Error: Test failure (8 shrinks):

  1. addPet : POST /pet {petId = 27, petName = qux}
  2. addPet : POST /pet {petId = 27, petName = qux}
    ↳ 409 Conflict: Pet already exists

  Use --seed -2917004710203612904 to reproduce
  ```
  Try running with `--no-shrinking` flag to see the original test case that
  failed.
  </details>

- [x] Coverage statistics per operation and response
  <details>

  <summary>Example</summary>

  ```
  Coverage:
    2xx:
      44% addPet (89)
      54% getPet (107)
    404:
      2% getPet (3)
    409:
      0% addPet (1)

  Total: 200

  Use --seed 2469868563532480199 to reproduce
  ```

  </details>

- [x] Don't stop if a potential problem is found, present all findings at the
      end of a test run
  <details>

  <summary>Example</summary>

  ```
  Test failure:

  1. getPet : GET /pet/923 -> Pet
    ↳ 404 Not Found
  ------------------------------------------------------------------------
  Test failure (8 shrinks):
  
  1. addPet : POST /pet {petId = 842, petName = foo}
  2. addPet : POST /pet {petId = 842, petName = foo}
    ↳ 409 Conflict: Pet already exists

  Use --seed 2469868563532480199 to reproduce
  ```

  </details>

- [x] Built-in specifications formatter
  <details>

  <summary>Example</summary>

  ```bash
  $ cat example/petstore-bad-formatting.spex
  component PetStore
    where

  addPet     : POST
    /pet Pet

  getPet :GET /pet/{ petId  :
    Int} ->
      Pet
  $ spex format example/petstore-bad-formatting.spex
  component PetStore where
  
  addPet : POST /pet Pet
  getPet : GET /pet/{petId : Int} -> Pet
  ```

  </details>

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

## Usage

Click the "▸ Example" buttons in the features list to see examples of how to
invoke `spex`.

## Contributing

Spex is at an early stage of development and there are many ways to help out,
here are some examples:

* Help out with issues and PRs;
* Contribute artwork under appropriate licenses (at least CC-BY-SA);
* Discuss and be helpful to others on:
  - IRC in the channel [#spex](https://web.libera.chat/#spex) on `irc.libera.chat`;
  - [Zulip](https://spex.zulipchat.com/).
* Contributed to releated projects:
  - [`spexup`](https://github.com/spex-lang/spexup), the installer for the Spex
    language;
  - [`spex-lang.github.io`](https://github.com/spex-lang/spex-lang.github.io),
    the Spex language website.

## License

BSD-style (see the file [LICENSE](LICENSE)).
