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
