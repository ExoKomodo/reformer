# Open Issues

## Setup linting

### Branch (not created) - [setup-lint](https://git.sr.ht/~jamesaorson/reformer/tree/setup-lint)

### Overview

Lint the various forms of code in the codebase.

### Instructions

Linting will be added to a makefile target for every language used by the project.

Add a [CI build manifest](../.builds) that will also lint every commit.

### Acceptance Criteria

Be able to lint from the

- codebase using `make`
- container used in `container-{build,run}`
- [pipelines on Sourcehut](https://builds.sr.ht/~jamesaorson/reformer)

## Migrate from guile-sqlite to guile-dbi

### Branch (not created) - [guile-dbi](https://git.sr.ht/~jamesaorson/reformer/tree/guile-dbi)

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

## Login using username

### Branch (not created) - [login-with-username](https://git.sr.ht/~jamesaorson/reformer/tree/login-with-username)

### Overview

Currently, users are distinguished by password. I won't even explain why this is bad. Allow for username to be added.

### Instructions

- Add a username field to the form for submitting a post
- Match username when submitting a post
- Return a `401` when failing to submit
- (optional) Save the login in a client-side storage

### Acceptance Criteria

Be able to specify user when submitting a post
