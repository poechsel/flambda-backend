# flambda-backend
The Flambda backend project for OCaml.

## One-time setup for dev work or installation

Only currently tested on Linux/x86-64 and macOS/x86-64.

One-time setup:
```
$ opam switch 4.11.1  # or "opam switch create 4.11.1" if you haven't got that switch already
$ eval $(opam env)
$ git clone https://github.com/ocaml-flambda/dune
$ cd dune  # We'll refer to this "dune" directory below as $DUNE_DIR
$ git checkout origin/special_dune
$ make release
```

You probably then want to fork the `ocaml-flambda/flambda-backend` repo to your own Github org.

## Branching and configuring

Use normal commands to make a branch from the main upstream branch (currently `4.11`), e.g.:
```
$ git clone https://github.com/ocaml-flambda/flambda-backend
$ cd flambda-backend
$ git checkout -b myfeature origin/4.11
```

The Flambda backend tree has to be configured before building.  The configure script is not checked
in; you have to run `autoconf`.  For example:
```
$ autoconf
$ ./configure --prefix=/path/to/install/dir --enable-middle-end=closure --with-dune=$DUNE_DIR/dune.exe
```
You can also specify `--enable-middle-end=flambda`.

## Building and installing

To build and install the Flambda backend, which produces a compiler installation directory whose
layout is compatible with upstream, run:
```
$ make  # or e.g. make -j16
$ make install
```

## Running tests

Prior to `make install` you can do:
- `make runtest` to run the Flambda backend tests (which use dune);
- `make runtest-upstream` to run the upstream testsuite. The upstream
testsuite runs much faster if you install GNU parallel. This is likely
already present on Linux machines. On macOS, install Homebrew, then `brew
install parallel`.
- `make compare` to run the comparison script that finds differences
between the upstream and Flambda backend install trees.  This script currently
only runs on Linux, although it shouldn't be hard to port to macOS, especially
if using GNU binutils.  It is recommended to install the Jane Street `patdiff` executable
before running `make compare`.

There is also a `make ci` target (best run as e.g. `make -j16 ci`) which does a full build
and test run.  It does not currently run `make compare` as some spurious failures have
been observed on the Github systems.

## Rebuilding during dev work

To rebuild after making changes, you can just type `make` (or `make -j16`, etc).

There is a special target `make hacking` which starts Dune in polling mode.  The rebuild
performed here is equivalent to `make ocamlopt` in the upstream distribution: it rebuilds the
compiler itself, but doesn't rebuild the stdlib or anything else with the new compiler.
This target is likely what you want for development of large features in the middle end or
backend.  Rebuild times for this target should be very fast.

The aim is to minimise patches against the upstream compiler (the contents of
the ocaml/ subdirectory), but you can configure and build in that directory as
you would for upstream.  If a bootstrap is required, the normal bootstrapping
commands should also work (all from within the ocaml/ subdirectory); the
newly-bootstrapped compiler will be picked up the next time that the Flambda
backend is built from the toplevel directory of the checkout.

