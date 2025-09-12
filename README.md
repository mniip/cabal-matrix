# cabal-matrix

`cabal-matrix` is a matrix builder for cabal. If you have a cabal project,
`cabal-matrix` lets you specify in a flexible way a list of configurations in
which you may want to build your project, such as compiler versions, or
dependency version restrictions. It will then run the builds in parallel
(locally), and present the results in a TUI table where the specific outcomes
can be more closely examined. This is useful for inventing dependency bounds for
your package, and verifying that the dependency bounds are correct.

# Example Usage

To run `cabal-matrix` you need to have a cabal project in the current directory,
buildable with `cabal build` or `cabal build <target>`.

If you have `ghc-9.6.7` and `ghc-9.8.4` on your `PATH`, you can run
```sh
cabal-matrix -j1 --compiler ghc-9.6.7,ghc-9.8.4 [target]
```
This will present you with a table that you can navigate using the TUI
keybindings at the bottom of the screen.

If your package depends on e.g. `text` and `bytestring` you can get a more
exciting build matrix with:
```sh
cabal-matrix -j1 --package text=2.0,2.1,2.1.3 --times --package bytestring=0.10.0.0,0.11.0.0,0.12.0.0,0.12.2.0
```
This will build a cartesian product of 3 `text` versions and 4 `bytestring`
versions, for a total of 12 builds.

Unless you are debugging a dependency issue between `text` and `bytestring` you
probably don't want to build the full combinatorial explosion of your package
with each version of `text` AND each version of `bytestring`. Instead of a
cartesian product you can do the union:
```sh
cabal-matrix -j1 --package text=2.0,2.1,2.1.3 --add --package bytestring=0.10.0.0,0.11.0.0,0.12.0.0,0.12.2.0
```
This will run 3 + 4 builds: for each version of `text` with `bytestring`
unrestricted, and for each version of `bytestring` with `text` unrestricted.

These 7 builds can be cartesian producted by 2 compiler versions from before:
```
cabal-matrix -j1 --[ --package text=2.0,2.1,2.1.3 --add --package bytestring=0.10.0.0,0.11.0.0,0.12.0.0,0.12.2.0 --] --times --compiler ghc-9.6.7,ghc-9.8.4
```
This will run (3 + 4) * 2 builds. The `--[` and `--]` specify operator
precedence.

In this case the table might look something like this:
```
text      │2.0        2.1        2.1.3                                                
bytestring│                                 0.10.0.0   0.11.0.0   0.12.0.0   0.12.2.0 
──────────┼───────────────────────────────────────────────────────────────────────────
COMPILER  │
──────────┼───────────────────────────────────────────────────────────────────────────
ghc-9.6.7 │build ok   build ok   build ok   no plan    no plan    no plan    build ok 
ghc-9.8.4 │no plan    build ok   build ok   no plan    no plan    no plan    build ok  
```

You can press `x` and reconfigure the axes for a more convenient view:
```
COMPILER        │ghc-9.6.7  ghc-9.8.4 
────────────────┼─────────────────────
text  bytestring│
────────────────┼─────────────────────
2.0             │build ok   no plan   
2.1             │build ok   build ok  
2.1.3           │build ok   build ok  
      0.10.0.0  │no plan    no plan   
      0.11.0.0  │no plan    no plan   
      0.12.0.0  │no plan    no plan   
      0.12.2.0  │build ok   build ok  
```