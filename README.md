# [`reformer.fyi`](https://reformer.fyi)

## Important Links

- [Source code](https://git.sr.ht/~jamesaorson/reformer)
- [Source code mirror](https://github.com/exokomodo/reformer)
- [External tool/library documentation](./external/docs/)

## Setup

To setup a system, `root` access is often required. So, if using `sudo`:

```shell
sudo make setup
```

And if not:

```shell
make setup
```

## Running

### As in production

```shell
make run
```

### In a REPl

```shell
make repl
# Now either (use-module) or copy-paste relevant code to the repl
```

### In a container

```shell
make container-build container-run
```

or using bash magic

```bash
make container-{build,run}
```
