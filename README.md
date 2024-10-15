# Spex

Spex is a programming language for working with specifications.

## Quick start

```bash
git clone 
cd
nix-shell
cabal run petstore & # Start demo application in the background.
cabal run spex -- --file example/petstore.spex
fg # Bring demo application to the foreground.
^C # Stop the demo application with ctrl-c.
```

## Contributing

### To do

- Health check
- SUT
- Coverage-guided fuzzing
  + use responses in generation
- Shrinking
- Packaging
  + cabal freeze
  + static binary?
- docs: contributing.md

- nice cli
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
- [ ] Protocol specification and run-time checking
- [ ] Models
- [ ] Refinement / versioning?
- [ ] Views?
- [ ] Async specification
- [ ] Templating using Lua?
