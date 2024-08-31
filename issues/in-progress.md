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

## Load and store models in Sqlite

### Branch - [rw-sqlite-models](https://git.sr.ht/~jamesaorson/reformer/tree/rw-sqlite-models)

### Overview

We need to be able to actually use the database to store our data. Right now there is no translation layer.
Read and write models to and from a Sqlite db.

### Instructions

Add new functions to the existing `<post>` and `<user>` objects.

### Acceptance Criteria

Have info about:

- load models from sqlite
- display posts from actual user data
- use the store functions for filling the test entries for dev

