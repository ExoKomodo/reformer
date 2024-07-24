# Open Issues

## Setup ASDF project

### Branch (not created) - [setup-asdf](https://git.sr.ht/~jamesaorson/reformer/tree/setup-asdf)

### Overview

[`ASDF`](https://asdf.common-lisp.dev/) is the ubiquitous build facility/project management tool for Common Lisp.

### Instructions

Set up the base project/system definition required by `ASDF`, and store the docs local to our project.

### Acceptance Criteria

- Add `ASDF` to the `deps` target in the [`Makefile`](../Makefile).
- Add `ASDF` manual to [`external/docs/`](../external/docs).
- Create [`reformer.asd`](../reformer.asd).
