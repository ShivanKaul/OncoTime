OncoTime
=========

OncoTime is an experimental Domain Specific Language which allows for easy analysis and visualization of patient treament paths in radiation oncology.


Group Name: Dragon Army

- Brendan Games Gordon - 260529254
- Shivan Kaul Sahib - 260512593
- Yusaira Khan - 260526007

We have set up a number of scripts to help with building and running. We have

```
build.sh - run this to build the project, and get a copy of the executable in the top-level folder
tests.sh - runs the tests (it runs the compiler on programs in programs/valid/ and programs/invalid/)
everything.sh - runs both of the above two scripts
```

We have placed a copy of the `oncotime` executable in the top-level folder. Note that this is compiled for Darwin.

## To Build

We can use either the `build.sh` script, or:
```
cabal sandbox init
cabal install --only-dependencies
cabal build
```

Note: we use some functions that are available starting GHC version 4.8.
We used some of Vikram's test cases that were on the GitHub Oncotime repo.

## To Run
- The compiled compiler is compiled into the `dist/build/oncotime` directory
- We've kept a copy of an executable at the top-level. Simply run `./oncotime <filename>`
