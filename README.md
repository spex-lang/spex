# Spex

Spex is a programming language for working with specifications.

## Quick start

```bash
git clone https://github.com/stevana/spex.git
cd spex
nix-shell
cabal run petstore & # Start demo application in the background.
cabal run spex -- --file example/petstore.spex
fg # Bring demo application to the foreground.
^C # Stop the demo application with ctrl-c.
```

## Feature list

* Concise specification language for HTTP services, e.g.:

  ```
  component PetStore where

  addPet : POST /pet Pet
  getPet : GET /pet/{petId : Int} -> Pet

  type Pet =
    { petId   : Int
    , petName : String
    }
  ```

* Ability to test specification against a deployment, without
  providing a model. 

* Keep track of previously generated values and sometimes try to use them
  during generation of new tests. For example, without this ability the
  `getPet` requests would all most certainly return 404.

* Ability to annotate inputs with @ and !...

## Feature wish list

* Keep track of previous responses and try to use them during generation 

* Coverage statistics and use coverage-guidance (endpoint coverage,
  but also check coverage of the range of values from responses.)

* User provided coverage and minimal test cases

* Time-traveling debugger for fail test cases

* Lint the spec, e.g. can all commands be reached? Or does some
  command have a parameter which no other command returns and we
  cannot generate using the built-in types?

* Ability to import/export OpenAPI (and later protobuf)

* Use templating and (Lua?) extensions for doc/code generation from
  spec, a bit similar to how pandoc does it.

* Visualise by generating diagrams

* REPL which generates data on tab-complete

* Ability to specify protocols, e.g. which sequences of commands
  are valid, and use this to do "run-time session type" checking

* Async specs where each component can be annotated with "produces
  events" and "consumes events", which can be visualised and linted
  for e.g. events that nobody consumes

* Something about versioning, upgrades, refinement of specs...


## Contributing

### Short term

- Health check
- Shrinking
- Packaging
- use duration rather than numTests?
- docs: contributing.md

### Refactor

- remove use of String
- pretty print
- structured logs, returns json object with one error or one test result?
- move Prng to AppEnv?

### Longer term

- nice cli
  + https://hackage.haskell.org/package/diagnose-2.5.1/docs/Error-Diagnose.html
  + https://medium.com/designing-atlassian/10-design-principles-for-delightful-clis-522f363bac87
  + https://github.com/charmbracelet/bubbletea
  + https://gleam.run/

- [ ] cli commands
  + test/verify
  + version
  + visualise
  + docs
  + fmt
- [ ] Import/export: OpenAPI, protobuf, modules/headers, ...?
- [ ] Visualise
  + Actants and c4 model?
- [ ] REPL, generate data with tab complete
- [ ] Models?
  + generate prototype from model
- [ ] Views?
- [ ] Dog fooding, test itself?
  + show me all the minimal test cases that lead to each error?
