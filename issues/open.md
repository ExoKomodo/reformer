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

