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

## Load and store data in SQLite

### Branch (not created) - [setup-sqlite](https://git.sr.ht/~jamesaorson/reformer/tree/setup-sqlite)

### Overview

Add a SQLite database to Reformer, for storing persistent data and allowing for us to be stateless.

### Instructions

1. Add a makefile target for spinning up a sqlite database
1. Add a makefile target for tearing down a sqlite database
1. Make the `run` and `run-with-lb` targets be aware of whether or not sqlite is up
(idea: have the makefile require a sqlite lock file as a dependency)
1. Create a SQLite database in the deployed instance

### Acceptance Criteria

1. Restarting a running app should come back up with prior state.
    - In DO, until we add a digital volume, this may be tricky
    - Test this locally for now
    - Write an automated test for this
1. SQLite target should be easily retargetable (locally run, accessed over a network, etc).
1. Local dev and container dev must remain unhindered or be somehow improved.
    - Consider running docker compose with sqlite as a separate container.
    - Allows for easy disaster testing scenarios of a local database backing up data to a cloud-networked database,
    then doing failovers and restores.
