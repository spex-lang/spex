# Changelog for Spex

## 0.0.0 -- 2024-11-19

First release, mostly focusing on setting up the infrastructure while also
providing a minimal specification language and toolkit for working with toy
HTTP JSON APIs.

### Infrastructure 

- CI/CD for Linux, MacOS and Windows;
- Static binaries for Linux x86_64;
- CI pipeline can be run locally using `make`.

### Specification language

- Convenient syntax for specifying HTTP JSON APIs;
- The currently only allowed base types are `String` and `Int`. Composite
  types can be constructed using records, and user defined type aliases are
  allowed;
- Two modal types, called abstract (@) and unique (!), which help guide the
  random generation of values of said types. Abstract types are not generated
  but rather reused from previous responses, and unique types are always
  generated and never reused. See documentation for more details and
  examples.

### Specification toolkit

The toolkit lets you do useful stuff with specifications. All these tools are
accessed via the `spex` command-line utility.

#### Verify

- Takes a specification and an URL to a HTTP API server and checks if the
  server respects the specification;
- Outputs minimised/shrunk test cases that lead to:
  + Non-2xx responses, or;
  + JSON response decode or type errors.
- Outputs unreachable endpoints and basic coverage statistics.
- The number of generated operations can be adjusted using `--tests` flag;
- Shrinking can be turned off with `--no-shrinking` flag;
- Test runs can be reproduced by passing `--seed` flag.

#### Format

- Parses and pretty prints a specification, thereby formatting it into standard
  form.

#### Check

- Parses a specification and checks it for internal inconsistencies:
  + Syntax errors;
  + Scope errors (types are used that are not defined).

#### Mock

- Takes a specification and starts a mock server which will reply to requests
  using random responses;
- Mocks can be reproduced using the `--seed` flag.

### Spexup

The Spex specification language and toolkit installer.

- Installs the latest `spex` command-line utility on x86_64-linux, arm64-macos
  and x86_64-windows, as well as `spex-demo-petstore` example HTTP server. The
  `spex-demo-petstore` binary allows you to follow along the documentation
  without having to develop an example of your own.
