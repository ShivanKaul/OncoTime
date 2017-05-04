OncoTime
=========

OncoTime is an experimental Domain Specific Language which allows for easy analysis and visualization of patient treament paths in radiation oncology.

This implementation was undertaken as the project for COMP 520: Compiler Design taught by [Laurie Hendren](https://en.wikipedia.org/wiki/Laurie_Hendren) at McGill University. Our compiler is written in Haskell using the [Parsec](https://wiki.haskell.org/Parsec) parser combinator library and compiles OncoTime programs down to JavaScript.

Our compiler's name is Doc.

We have set up a number of scripts to help with building and running. We have

```
build.sh - run this to build the project, and get a copy of the executable in the top-level folder
tests.sh - runs the tests (it runs the compiler on programs in programs/valid/ and programs/invalid/)
everything.sh - runs both of the above two scripts
```

## To Build

We can use either the `build.sh` script, or:
```
cabal sandbox init
cabal install --only-dependencies
cabal build

# To install node dependencies
npm install
```

The flags are supported and can be used thusly:
```
./doc <filename> --pptype --dumpsymtab
```

We also support `-h / --help` and `-v / --version` flags.

Note that we have a `config.conf` file. This file must be at the top-level directory i.e. must be in the same directory as the `doc` executable.

## To Run
- The compiled compiler is compiled into the `dist/build/doc` directory
- We've kept a copy of an executable at the top-level. Simply run `./doc <filename>`
