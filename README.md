OncoTime
=========

Group Name: Dragon Army

## To Build
```
cabal sandbox init
cabal install --only-dependencies
cabal build
```

Note: we use some functions that are available starting GHC version 4.8.

## To Run
- The compiled compiler is compiled into the dist/build/oncotime directory
- We've kept a copy of an executable at the top-level. Simply run `./oncotime <filename>`
