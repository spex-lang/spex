# To do

Here's what I'm currently working on:

### Features

- Health check
  + make the output nicer
- use duration rather than numTests?
- print progress while testing
- docs: contributing.md
  + https://github.com/rubyberlin/code-of-conduct?tab=readme-ov-file#readme ?
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
    * https://github.com/reproducible-containers/buildkit-cache-dance
    * https://dev.doroshev.com/blog/docker-mount-type-cache/
  + install script?
    * confirm with user
    * update spexup itself? 
    * cache releases.json?
    * check what versions are locally installed already

      spexup [update] [spexup|spex] -- install latest version
    * spexup list -- lists available releases
      spexup install (spexup|spex) <version>

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
