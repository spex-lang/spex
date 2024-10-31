# Roadmap for future releases

* Coverage-guidance and mutation

* User provided coverage and minimal test cases
  + E.g. show me all the minimal test cases that lead to each error?

* Check coverage of the range of values from responses?

* Support for other content types than `application/json`

* Ability to import/export OpenAPI (and later protobuf)

* Editor support

* Only presents new errors, abstract counterexamples like in "Find More Bugs
  with QuickCheck!" paper?

* Optional models?

* Generate prototype from model?

* Time-traveling debugger for fail test cases

* REPL that can generate data on tab complete
  - https://abhinavsarkar.net/posts/repling-with-haskeline/
  - https://github.com/Gabriella439/grace/blob/main/src/Grace/REPL.hs

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

