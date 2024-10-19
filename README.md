# Spex

Spex is a programming language for working with specifications.

> [!CAUTION]
> Spex is pre-alpha and not suitable for general use yet.

## Quick start

```bash
git clone https://github.com/spex-lang/spex.git
cd spex
nix-shell
cabal run petstore & # Start demo application in the background.
cabal run spex -- --file example/petstore.spex
fg # Bring demo application to the foreground.
^C # Stop the demo application with ctrl-c.
```

## Feature list

* Concise specification language for HTTP services
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

* Ability to test specification against a deployment
  <details>
  <summary>Example</summary>
  ```bash
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
  ```
  </details>

* Keep track of previously generated values and sometimes try to use them
  during generation of new tests. For example, without this ability the
  `getPet` requests would all most certainly return 404.

* Ability to annotate input types with abstract and unique modalities (@ and
  !), e.g.:
  <details>
  <summary>Example</summary>
```diff
  $ diff -u example/petstore-basic.spex example/petstore-modal.spex
  - addPet : POST /pet Pet
  - getPet : GET /pet/{petId : Int} -> Pet
  + addPet : POST /pet !Pet
  + getPet : GET /pet/{petId : @Int} -> Pet
```
  </details>

  Where an abstract type isn't generated, i.e. gets reused, and a unique type
  is always generated and never reused. Without any annotation a coin is
  flipped and the value either gets reused or generated.

## Feature wish list for first release

* Keep track of previous responses and try to use them during generation 

* Nice CLI and errors
  + https://elm-lang.org/news/compiler-errors-for-humans
  + https://hackage.haskell.org/package/diagnose-2.5.1/docs/Error-Diagnose.html
  + https://medium.com/designing-atlassian/10-design-principles-for-delightful-clis-522f363bac87
  + https://github.com/charmbracelet/bubbletea
  + https://gleam.run/

* Coverage statistics and use coverage-guidance (endpoint coverage,
  but also check coverage of the range of values from responses.)

* Shrinking

* User provided coverage and minimal test cases
  + E.g. show me all the minimal test cases that lead to each error?

* Formatting of specs

* Don't stop if error is found

* Only presents new errors?

## Feature wish list for later releases

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

* Use templating and (Lua?) extensions for doc/code generation from
  spec, a bit similar to how pandoc does it.

* Visualise by generating diagrams
  + only makes sense if we have a bigger system out of multiple components and
    some relations between them?
  + complex systems approach? (actants, constructors)
  + c4 model?

* REPL which generates data on tab-complete

* Ability to specify protocols, e.g. which sequences of commands
  are valid, and use this to do "run-time session type" checking

* Async specs where each component can be annotated with "produces
  events" and "consumes events", which can be visualised and linted
  for e.g. events that nobody consumes

* Temporal logic on events? E.g. something like `if buyPet then eventually
  paymentEvent`

* Something about versioning, upgrades, refinement of specs...

## Contributing

If any of the above sounds interesting, or you have ideas of your own, feel
free to open a ticket!

For simplicity, while it's just me working, I'll just keep the issues here in
the README. I'll clean this up and create proper tickets for the 0.0.0 release.

Here's what I'm currently working on:

### Features

- Health check
- use duration rather than numTests?
- docs: contributing.md
- Packaging
  + ci release job
  + install script?
  + changelog generator (semantic commit messages)
  + commit hook for conventional commits?

### Bugs

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
