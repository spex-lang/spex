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

## Feature wish list

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
  providing a model. Keep track of responses and try to use them
  during generation and use coverage-guidance (endpoint coverage,
  but also check coverage of the range of values from responses.)

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

### To do

- Coverage-guided fuzzing
  + reuse generated fields
  + use responses in generation
  + abstract types, that don't get generated?
  + unique types, always get generated?
- Health check
- Shrinking
- Packaging
  + cabal freeze
  + static binary?
- docs: contributing.md
- remove use of String
- pretty print
- use duration rather than numTests?
- structured logs, returns json object with one error or one test result?

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
- [ ] Import/export: OpenAPI, protobuf, modules/headers, ...?
- [ ] Visualise
  + Actants and c4 model?
- [ ] REPL, generate data with tab complete
- [ ] Pretty print aka fmt
- [ ] Models?
  + generate prototype from model
- [ ] Views?
- [ ] Dog fooding, test itself?
  + show me all the minimal test cases that lead to each error?
