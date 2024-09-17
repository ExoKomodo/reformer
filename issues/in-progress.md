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

## Migrate from guile-sqlite to guile-dbi

### Branch - [guile-dbi](https://git.sr.ht/~jamesaorson/reformer/tree/guile-dbi)

### Overview

Reformer needs to support more than one DB backend. Initially, supporting SQLite and PSQL will be most necessary.

Using `guile-dbi` to connect to Sqlite and Postgres will let us use
the same API for connecting to all DB backends.

### Instructions

Install [`guile-dbi`](https://github.com/opencog/guile-dbi) to the project. If at all possible, provide the source code
of this library natively in the codebase. If necessary to build the code first in some way, then introduce it as a git
submodule and provide a `make` target for dealing with library compilations.

### Acceptance Criteria

- replace `guile-sqlite` with `guile-dbi`
- if necessary to build `guile-dbi`, add a `make build-libs` target
