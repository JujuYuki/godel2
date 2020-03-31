# godel2 [![Docker Hub](https://img.shields.io/badge/docker-ready-blue.svg)](https://hub.docker.com/r/jgabet/godel2/)

## Go Model Checker for MiGo types

Godel 2 is a model checker for MiGo types, extending [Godel Checker](https://bitbucket.org/MobilityReadingGroup/godel-checker)

### Run via Docker

There are many external dependencies in this tool, the easiest way to get
started is to run via a [Docker](http://www.docker.com) container.

The docker container contains all dependencies build with the latest version of
`Godel`, where the container is built from latest commits. Given an example cgo
file, for example:

    /tmp/example.cgo

Run the example with:

    docker run -ti --rm -v /tmp:/root jgabet/godel2:latest Godel example.cgo    # Model check
    docker run -ti --rm -v /tmp:/root jgabet/godel2:latest Godel -T example.cgo # Termination check

The argument `-v /tmp:/root` puts the `/tmp` directory into the container as
`/root`, so the `example.cgo` file is found in the container as
`/root/example.cgo`.

Alternatively, use the `docker-run` script to run the examples:

    cd /path/to/example;
    docker-run example.cgo

This puts `/path/to/example` into docker and run the example inside the container,
all arguments are passed to the `Godel` binary inside the container, e.g.

    docker-run -T example.cgo # Term check

### Development Environment via Docker

This is similar to above, except the `Godel` binary in the current directory
will be used instead, so small modifications can be tested immediately. The
termination checker and its dependencies are in the container.

    dev-run example.cgo

### Build from scratch

Instructions to build from scratch can be found below.
There are three components: `Godel` frontend, the model checker backend,
and the termination analyser backend.

#### Godel

The `Godel` frontend is written in Haskell.

**Dependencies**

- Glasgow Haskell Compiler (`ghc`) 8.0.2
- The `cabal` package manager

For Ubuntu Linux, `ghc` can be installed via `apt-get` as part of Haskell
Platform, and the packages can be installed via `cabal`.

    $ sudo apt-get install haskell-platform

Then run cabal update and install:

    $ cabal update && cabal install --bindir=.

The `Godel` binary should be created after a successful build.

#### Model checker

The model checker we use is
[mCRL2](http://www.mcrl2.org/web/user_manual/index.html) (2018 release),
and can be downloaded from their official website (recommended).

#### Termination checker

We use the [KITTeL/KoAT](https://github.com/s-falke/kittel-koat) termination
prover and its [llvm2KITTeL](https://github.com/s-falke/llvm2kittel) LLVM frontend.

To build them from source, we need the following dependencies

- clang/LLVM 2.9+ (development packages)
    * Obtainable from http://releases.llvm.org/download.html
- GNU Multiprecision library (GMP) version >= 5.0.0
    * Needed for Yices and LLVM translation, obtainable from https://gmplib.org/
- Yices SMT solver
    * We require specifically version 1 (not Yices2)
- OCaml

First download Yices (version 1), visit
http://yices.csl.sri.com/old/download-yices1.shtml to obtain the package,
extract and make the `yices` binary available in `$PATH`.

On Ubuntu, the other dependencies can be installed via `apt-get`:

    $ apt-get install clang llvm-dev \
      ocaml-nox camlp4 libocamlgraph-ocaml-dev camlidl \
        libapron-ocaml-dev libedit-dev zlib1g-dev \
      libgmp-dev

Then build `llvm2kittel` and `kittelkoat` from scratch, and make the binary
available in `$PATH` (assuming `/usr/local/bin` is in `$PATH`):

    $ git clone https://github.com/s-falke/kittel-koat; \
        cd kittel-koat; make; cd .. ; \
        cp kittel-koat/_build/kittel.native /usr/local/bin/ # Move to /usr/local/bin

    $ mkdir build; \
        git clone https://github.com/s-falke/llvm2kittel; \
        cd build; cmake ../llvm2kittel && make; cd ..; \
        cp build/llvm2kittel /usr/local/bin/ # Move to /usr/local/bin

The `Godel` frontend will call the appropriate installed binaries.

