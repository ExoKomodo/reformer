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

## Load and store data in SQLite

### Branch - [setup-sqlite](https://git.sr.ht/~jamesaorson/reformer/tree/setup-sqlite)

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
