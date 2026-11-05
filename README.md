This is the code for my blog on [modelling French sovereign debt in Haskell](https://dubovik.eu/blog/dsa-haskell).

The folder `data` contains precompiled Eurostat and ECB series. The folder `haskell` contains the incremental refactors described in the blog, with `refactor3-memoizaion.hs` being the final one. The folder `benchmarks` contains Haskell and Rust variants of the program, with basic built-in benchmarks.

The Makefiles are written for my machine. In particular, the Haskell code is compiled with the `-dynamic` flag. Adjustments might be needed for other configurations.
