# Open Issues

## Setup Clack

### Branch (not created) - [setup-clack](https://git.sr.ht/~jamesaorson/reformer/tree/setup-clack)

### Overview

[`clack`](https://github.com/fukamachi/clack) will be used as the server abstraction library for Reformer.
It seems that  using [`hunchentoot`](https://edicl.github.io/hunchentoot/) for development and
[`woo`](https://github.com/fukamachi/woo) for production is a good idea, according to `clack`'s creator.
Look further into this and decide on your own. It seems that the reasoning would be that `woo` is overkill for local.

### Instructions

1. Setup a root route to serve a basic home page
1. Add a `make` target to install [`roswell`](https://github.com/roswell/roswell)
1. Modify the `run` target to serve the app using [`clackup`](https://github.com/fukamachi/clack/blob/master/roswell/clackup.ros)

### Acceptance Criteria

- `/` should present a fully-compliant HTML page
- `clackup` is used to run the app locally, when not developing with `slime`
