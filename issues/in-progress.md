# In-Progress Issues

Once completed, move the issue into [closed](./closed.md).

## Create the initial README

### Branch - [initial-readme](https://git.sr.ht/~jamesaorson/reformer/tree/initial-readme)

### Overview

README is entirely lacking. Provide info about the vision and purpose of the site. Also provide basic development info.
For now, structure is more important than content.

### Instructions

Make a nice README, my guy.

### Acceptance Criteria

Have info about:

- the purpose of the website.
- technologies chosen (and why).
- the development process.

## Setup ASDF project

### Branch - [setup-asdf](https://git.sr.ht/~jamesaorson/reformer/tree/setup-asdf)

### Overview

[`ASDF`](https://asdf.common-lisp.dev/) is the ubiquitous build facility/project management tool for Common Lisp.

### Instructions

Set up the base project/system definition required by `ASDF`, and store the docs local to our project.

### Acceptance Criteria

- Add `ASDF` to the `deps` target in the [`Makefile`](../Makefile).
- Add `ASDF` manual to [`external/docs/`](../external/docs).
- Create [`reformer.asd`](../src/reformer.asd).
