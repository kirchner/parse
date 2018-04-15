# Parse

**Work in progress.**

## Build documentation

Install the [Nix package manager](https://nixos.org/nix), then run:

```sh
$ make
```

### Build example live-queries

```sh
$ cd examples/live-queries
$ make setup
$ make parse &
$ make
$ open ./index.html
```
