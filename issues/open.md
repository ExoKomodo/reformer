# Open Issues

## Public Domain

### Branch (not created) - [public-domain](https://git.sr.ht/~jamesaorson/reformer/tree/public-domain)

### Overview

Project should be public domain, ala [`The Dorean Principle`](https://thedoreanprinciple.org) found throughout scripture.

### Instructions

Add a public domain disclaimer to the codebase

### Acceptance Criteria

Do something like SQLite's `LICENSE.md`:

```markdown
The author disclaims copyright to this source code.  In place of
a legal notice, here is a blessing:

  *   May you do good and not evil.
  *   May you find forgiveness for yourself and forgive others.
  *   May you share freely, never taking more than you give.
```

## Add directory for external docs

### Branch (not created) - [external-docs](https://git.sr.ht/~jamesaorson/reformer/tree/external-docs)

### Overview

Many of the Lisp technologies used have comprehensive HTML manuals. It would be great to have these available for offline.
development.

### Instructions

Where possible, we should store these documents local to this project. We intend to eventually post the docs for
[`Reformer`](https://reformer.fyi) on a website, so put these in a location that would not be included. So do not use the
[`docs/`](../docs) directory, but rather [`external/docs/`](../external/docs).

### Acceptance Criteria

- Create the [`external/docs/`](../external/docs) directory.
- Link to this directory from the main [`README.md`](../README.md).

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
