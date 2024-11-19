# Spex

![Build status](https://github.com/spex-lang/spex/actions/workflows/main.yaml/badge.svg)

*Spex* is a specification language and toolkit for working with HTTP JSON APIs.
For more information see the [website](https://spex-lang.org).

> [!CAUTION]
> *Spex* is alpha and not suitable for general use yet. Please don't share just
> yet, I'll announce it soon.

## Why Spex?

* OpenAPI [specifications](https://www.openapis.org/), the most common way of
  specifying HTTP JSON APIs today, are written in JSON or YAML, both verbose and
  error-prone. *Spex* aims to provide a concise specification language which is a
  pleasure to read and write, as well as nice error messages that make it easy
  to fix mistakes;

* The OpenAPI [tooling](https://tools.openapis.org/) is extensive, and
  includes:

    - Generation of API clients, server stubs and documentation;
    - Testing / fuzzing;
    - Mocking;
    - Linting.

  *Spex*, despite still being young, already has support for fuzzing and
  mocking, as well as plans to add code generation, linting and some other
  tooling that doesn't have OpenAPI analogues:

    - REPL;
    - Debugger.

  Finally, one will also be able to import and export OpenAPI specifications,
  thereby having access to that ecosystem of tools as well.

  By coevolving the language and the tooling, we can add features that will be
  hard to replicate in OpenAPI, e.g.:

    - Refinement types -- validation logic;
    - Model definitions -- fakes rather than mocks and better fuzzing.

* Specifications of the HTTP JSON APIs of components in a system captures how
  the components may be called, but they don't say how the components are
  related to each other. Longer term *Spex* aims to allow for a wider range of
  specifications, e.g. async message passing, as well as means to compose them
  into bigger system specifications, this opens up for other kinds of tooling:

  - diagrams

For a more elaborate explaination of the motivation behind *Spex*, see the
following [link](https://spex-lang.org/motivation.html).

## Installation

See [`INSTALL.md`](INSTALL.md) for installation instructions.

## Usage

The easiest way to get started is to follow the
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
