# Spex

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

## Installation

### From precompiled binary

#### Automatic

> [!CAUTION]
> Not supported just yet...

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

## Roadmap for future releases

* Support for other content types than `application/json`

* Ability to import/export OpenAPI (and later protobuf)

* Editor support

* Optional models?

* Generate prototype from model?

* Time-traveling debugger for fail test cases

* REPL that can generate data on tab complete

* Lint the spec, e.g. can all commands be reached? Or does some
  command have a parameter which no other command returns and we
  cannot generate using the built-in types?

* Refinement types, e.g. `/pet/{petId : Int | petId > 0}` and ability to generate
  validation logic from them
  - [Refinement Types: A Tutorial](https://arxiv.org/abs/2010.07763v1) (2021)

* Use templating and (Lua?) extensions for doc/code generation from
  spec, a bit similar to how pandoc does it.

* Visualise by generating diagrams
  + only makes sense if we have a bigger system out of multiple components and
    some relations between them?
  + complex systems approach? (actants, constructors)
  + c4 model?

* REPL which generates data on tab-complete

* Ability to specify protocols, e.g. which sequences of commands
  are valid, and use this to do "run-time session type" checking.
  - https://www.youtube.com/watch?v=ed7A7r6DBsM
  - https://www.youtube.com/watch?v=FqlewYgUcZU

  - "We need languages to describe encodings and protocols not machine
    instructions" (https://www.youtube.com/watch?v=ieEaaofM7uU)

    start & open(File, Modes) ->
      {ok, Handle} & ready |
      {error, Reason} & closed.

    ready & close(Handle) ->
      ok & closed | 
      {error, Reason} & closed.

    ready & read(Handle, int) ->
      {ok, Bin} & ready |
      {error, E} & closed.

    start 
      & login : POST /login/{user : String} {password : String} -> {ok200, Token} & inside
                                                           | unauthorized401 & start
    inside & list : GET / -> List String & inside

    inside & logout : POST /logout & start
                                                          

* Async specs where each component can be annotated with "produces
  events" and "consumes events", which can be visualised and linted
  for e.g. events that nobody consumes

* Temporal logic on events? E.g. something like `if buyPet then eventually
  paymentEvent`

* Something about versioning, upgrades, refinement of specs...

* Other types of specifications, e.g. syntax grammars where testing generates
  random programs? And perhaps find minimal programs that create unique syntax
  errors? Use grammars as generators?

## Contributing

If any of the above sounds interesting, or you have ideas of your own, feel
free to open a ticket!

For simplicity, while it's just me working, I'll just keep the issues here in
the README. I'll clean this up and create proper tickets for the 0.0.0 release.

Here's what I'm currently working on:

### Features

- Health check
- use duration rather than numTests?
- print progress while testing
- docs: contributing.md
- docs: document syntax?

```
  Spec ::= "component" Ident "where" Decl*
    Decl ::= OpDecl | TypeDecl

      -- XXX: Allow ModalType in response position? 
      -- semantics: unique response type will never get reused, and abstract
      -- response type will always get reused?
      OpDecl ::= ident ":" Method Path Body? ("->" Type)? 

        Method ::= "GET" | "POST"

        Path ::= ("/" PathSegment)* "/"?

          PathSegment = "{" ident ":" ModalType "}" | path

        Body ::= "{" ModalType "}"

      TypeDecl ::= "type" Ident "=" Type

  ModalType ::= Mode? Type

  Mode ::= "@" | "!"

  Type ::= BaseType | RecordDecl | Ident

  BaseType ::= "Unit" | "Bool" | "Int" | "String"

  RecordDecl ::= "{" Field ("," Field)* "}"  -- XXX: parser allows empty records?
    Field ::= ident ":" Type                 -- XXX: Modal type


  Ident ::= [A-Z][a-zA-Z0-9]*
  ident ::= [a-z][a-zA-Z0-9]*

  -- https://datatracker.ietf.org/doc/html/rfc3986#section-3.3
  path ::= pchar+
    pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"

    pct-encoded   = "%" HEXDIG HEXDIG

    unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
    sub-delims    = "!" / "$" / "&" / "'" / "(" / ")"
                  / "*" / "+" / "," / ";" / "="
```

- Packaging
  + caching
    * https://github.com/moby/buildkit/issues/1673 
     cache /var/lib/buildkit?
    * https://dev.doroshev.com/blog/docker-mount-type-cache/
  + install script?
      spexup [update] [spexup|spex] -- install latest version
    * spexup list -- lists available releases
      spexup install (spexup|spex) <version>
    * https://raw.githubusercontent.com/haskell/ghcup-hs/refs/heads/master/scripts/bootstrap/bootstrap-haskell
    * https://install.determinate.systems/nix
  + changelog generator (semantic commit messages)
  + commit hook for conventional commits?
- https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/creating-and-highlighting-code-blocks#syntax-highlighting

### Bugs

- abstract types get generated, if there are not previous values to draw from.
  Fix by allowing generation to throw?
- unique types don't get checked
- normal types should sometimes generate/sometimes reuse

### Refactor

- rename CmdLineArgs to AppOptions?
- remove ExceptT
- remove use of String
- pretty print
- structured logs, returns json object with one error or one test result?
- move Prng to AppEnv?
- use formatter?
