# Spex

![Build status](https://github.com/spex-lang/spex/actions/workflows/main.yaml/badge.svg)

*Spex* is a specification language and toolkit for working with HTTP JSON APIs.
For more information see the [website](https://spex-lang.org).

> [!CAUTION]
> *Spex* is alpha and not suitable for general use yet. Please don't share just
> yet, I'll announce it soon.

## Why Spex?

* The *Spex* specification language strives to provide a concise way of
  specifying HTTP JSON APIs with Elm-style error messages;

* The *Spex* toolkit consists of various tools for working with specifications:
  - [x] A verifer: that checks a specfication against a server, using fuzzing and
    generative testing techniques, and reports back:
    + minimised test cases that lead to non-2xx responses, or;
    + JSON response decode or type errors, as well as;
    + any non-reachable APIs.
  - [x] A mock server: which takes a specification and creates a server which
    generates responses according to the specification;
  - [ ] Code generator: which takes a specification and a template and generates
    code, e.g. API clients, server stubs or documentation (this is not
    implemented yet).
  - [ ] A REPL: takes a specification and an URL and gives you a REPL which can be
    used to explore the server behind the URL, which tab completion generating
    random payload data for the different possible API calls (not implemented
    yet);
  - [ ] A debugger: which takes a test case and lets you step forwards and
    backwards one API call at the time and gives you a REPL to explore the
    system under test in between the steps (not implemented yet);
  - [ ] A import and exporter: which allows for conversion to and from other
    specification formats, e.g. OpenAPI (not implemented yet).

For a more elaborate explaination of the motivation behind *Spex*, see the
following [link](https://spex-lang.org/motivation.html).

## Installation

See [`INSTALL.md`](INSTALL.md) for installation instructions.

## Getting started

The easiest way to get started is to follow the installation instructions above
to obtain the `spex` and `spex-demo-petstore` binaries, and then clone this
repo to get access to the examples:

```bash
git clone https://github.com/spex-lang/spex.git
cd spex
spex-demo-petstore & # Start the demo HTTP API server in the background.
PETSTORE_PID=$! # Save the pid of the demo server, so we can kill it later.
spex verify example/petstore-modal-faults.spex --tests 2000
kill $PETSTORE_PID # Stop the demo server.
```

(You can also open the demo server in a separate terminal and avoid starting it
in the background and killing it.)

For a longer introduction, including what's happening behind the scenes when
you run e.g. `spex verify`, see the
[tutorial](https://spex-lang.org/tutorial.html).

## Community

If you got any questions or what to discuss what else we can do with
specification, come join one of our chats:

* [![Zulip](https://img.shields.io/badge/zulip-join_chat-blue.svg)](https://spex.zulipchat.com/)
* [![IRC: #spex on
  libera.chat](https://img.shields.io/badge/IRC-%23spex%20on%20libera.chat-blue.svg)](https://web.libera.chat/#spex)

Please make sure you follow the [code of conduct](.github/CODE_OF_CONDUCT.md)
when interacting with others.

## Contributing

*Spex* is at an early stage of development and is open to various forms of
contribution, see the [`CONTRIBUTING.md`](.github/CONTRIBUTING.md) file for
details.

## License

BSD-2-Clause (see the file [`LICENSE`](LICENSE)).
