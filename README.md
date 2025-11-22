# cabal-matrix

`cabal-matrix` is a matrix builder for cabal. It lets you specify in a flexible
way a list of configurations in which something can be built, such as compiler
versions, or dependency version restrictions. It will then run the builds in
parallel (locally), and present the results in a TUI table where the specific
outcomes can be more closely examined. This is useful for inventing and
correcting dependency bounds for your package, as well as finding dependency
issues in other packages and fixing them.

This README is a tutorial that walks through some example use cases. A reference
of CLI options can be obtained by running `cabal-matrix --help`.

# Installation

```sh
cabal install cabal-matrix
```

# Building Your Package

Suppose you have a package `foo` that you're developing as part of a cabal
project (with a `cabal.project` file in current directory), such that your
package can be built by running:
```sh
cabal build foo
```

You can then build your package with `cabal-matrix` using:
```sh
cabal-matrix -j1 foo --default
```

This will open a TUI with a single cell representing the single build you're
doing. You can use `<Tab>` and then `<Space>` to view and follow the output of
the build, and `q` or `<Esc>` to exit.

# Inventing Dependency Bounds

Suppose your package build-depends on `bytestring`, but you don't have any
version bounds in the cabal file, because you're not sure which to put. To
figure this out, we can simply try building our package with each version of
`bytestring` and see if it works or not! This can be achieved by running:
```sh
cabal-matrix -j1 foo --package bytestring
```

This may take a while to complete, and you can try using a higher value of `-j`
to run multiple builds in parallel, but in the end your table will look like
this (you can use arrow keys to scroll around):
```
bytestring│0.9        0.9.0.1    0.9.0.2    0.9.0.3    0.9.0.4    0.9.1.0    0.9
──────────┼────────────────────────────────────────────────────────────────────▶
          │
──────────┼────────────────────────────────────────────────────────────────────▶
          │no plan    no plan    no plan    no plan    no plan    no plan    no
          │
          │
          │
          │
<Esc>/Q: quit │ X: axes │ ◀▲▼▶: select cell │ <Tab>: focus cells/cols/rows
```

- A `build ok` result in a particular column means that your package built
  successfully against that version of `bytestring`, meaning your package de
  facto supports it.
- A `no plan` result is considered a tentative success. It means cabal could not
  come up with a build plan, because dependency bounds have prevented this
  configuration from being usable.
- A `build fail` result means that we tried building but there was an error.
  You can select the cell with arrow keys, and use `<Space>` to view the build
  output to see what the error was.
- A `deps fail` result means there was a problem building the dependencies.
  This could indicate a bug in `bytestring`, or in a package that is between
  `foo` and `bytestring` in the dependency graph. Or it could be that your build
  environment is not working properly.

The goal of setting version bounds is turning `build fail`s into `no plan`s:
you can choose the `bytestring` version range to be one that includes all the
`build ok`s, and excludes all `build fail`s.

You could also change your package to avoid whatever it was that caused the
`build fail`s in the first place, and thus widen the possible version range.

# Validating Dependency Bounds

Once you add bounds in the cabal file, or if you had pre-existing bounds, you
can verify that these bounds are not too loose. Suppose your bounds were
`build-depends: bytestring >=0.11.4 && <0.13`. You can then run:
```sh
cabal-matrix -j1 foo --package bytestring=">=0.11.4 && <0.13"
```
You could even run the same command as before (`--package bytestring`), but this
will save time (and table space!) by not focusing on configurations that are
definitely outside the possible range.

In the result, you should expect to see only `build ok`s and `no plan`s. As
before, `build fail` suggests that your bounds aren't tight enough, and
`deps fail` suggests an issue upstream (but you should verify).

Note however that `no plan` is only a tentative success. It could be caused by
bounds created by you, or by constraints not created by you. In the latter case
you run the risk of those constraints disappearing, and uncovering a
`build fail` underneath. A very common way this can happen is with compiler
versions, for example if you're using GHC 9.12.2, then any configuration
involving `bytestring-0.11.4.0` will be `no-plan` because `bytestring` itself
doesn't support that compiler. If I switch to GHC 9.8.4, `bytestring-0.11.4.0`
becomes buildable.

If you intend to support multiple compilers in your package, and you have e.g.
`ghc-9.8.4` and `ghc-9.12.2` on your `PATH`, you can test this scenario using:
```sh
cabal-matrix -j1 foo \
  --compiler ghc-9.8.4,ghc-9.12.2 \
  --times \
  --package bytestring=">=0.11.4 && <0.13"
```

This will create a build matrix that looks like this:
```
  COMPILER│ghc-9.8.4  ghc-9.12.2
──────────┼─────────────────────────────────────────────────────────────────────
bytestring│
──────────┼─────────────────────────────────────────────────────────────────────
0.11.4.0  │build ok   no plan
0.11.5.0  │build ok   no plan
0.11.5.1  │build ok   no plan
0.11.5.2  │build ok   no plan
0.11.5.3  ▼build ok   build ok
<Esc>/Q: quit │ X: axes │ ◀▲▼▶: select cell │ <Tab>: focus cells/cols/rows
```

If you like the view transposed, you can press `X` and configure "COMPILER" to
use the Vertical axis and "bytestring" to use the Horizontal axis instead.

Note that in the invocation above, `--compiler ...` produces a list of
compilers, and `--package bytestring=...` produces a list of `bytestring`
versions, and the `--times` operator between them combines the two lists using a
cartesian product.

If your package also depends on e.g. `text`, you may want to similarly check
which versions of `text` it actually builds with, with e.g.
```sh
cabal-matrix -j1 foo \
  --compiler ghc-9.8.4,ghc-9.12.2 \
  --times \
  --package bytestring="==0.11.5.*" \
  --times \
  --package text="==2.0.*"
```
This will run a build for each of the 2 compilers, for each of the 5 selected
`bytestring` versions, and each of the 3 selected `text` versions for a total
of 2 * 5 * 3 = 30 builds.

For most situations this is wasteful though, as independent packages don't
usually interact in a way that will only cause a problem for a combination of
versions. For dependent packages, such as `text` depending on `bytestring`, the
incompatibility will usually manifest when building `text`, which is why
`text` will have its own dependency bounds for `bytestring`. And if those are
incorrect then that's a problem in `text`.

So instead it is usually sufficient to only constrain one package at a time.
You can do this with `cabal-matrix` like so:
```sh
cabal-matrix -j1 foo \
  --compiler ghc-9.8.4,ghc-9.12.2 \
  --times \
  --[ \
  --package bytestring="==0.11.5.*" \
  --add \
  --package text="==2.0.*" \
  --]
```
Note the use of `--add` here instead of `--times` -- this concatenates the two
lists instead of taking their cartesian product. Also note the use of `--[`
`--]` to group the operands together, otherwise operations are executed from
left to right (a.k.a. left associatively).

The above invocation will run 2 * (5 + 3) = 16 builds instead. You can press `X`
and configure "bytestring" and "text" to use the Vertical axis, and then the
table will look like this:
```
        COMPILER│ghc-9.8.4  ghc-9.12.2
────────────────┼───────────────────────────────────────────────────────────────
bytestring text │
────────────────┼───────────────────────────────────────────────────────────────
0.11.5.0        │build ok   no plan
0.11.5.1        │build ok   no plan
0.11.5.2        │build ok   no plan
0.11.5.3        │build ok   build ok
0.11.5.4        │build ok   build ok
           2.0  │no plan    no plan
           2.0.1│build ok   no plan
           2.0.2│no plan    no plan
<Esc>/Q: quit │ X: axes │ ◀▲▼▶: select cell │ <Tab>: focus cells/cols/rows
```

Note that there are unusual circumstances where a simple `--add` will be
insufficient, particularly if you use `#ifdef`s or cabal flags to faciliate a
migration for some breaking change. It's your job to know whether your package
uses such mechanisms, to decide how much they should be tested, and to find a
balance between practicality and the combinatorial explosion of configurations.

# Parallelism

There are two ways to specify parallelism: you can specify `-jN` to tell
`cabal-matrix` to run N cabals in parallel, and you can also use `--option -jM`
to tell `cabal-matrix` to *forward* the option `-jM` to cabal, causing cabal to
use M threads in turn. In combination this may require up to N * M cores.

I recommend the combination `-jN --option -j1`, where `N` is the number of cores
on your machine; because planning isn't threaded, and because `--option -j1`
also causes cabal to log each module being compiled, rather than only entire
packages.

# Building Someone Else's Package

If you run into `deps fail`, or otherwise find a package on Hackage that fails
to build because its dependency bounds are incorrect, you can easily debug this
with `cabal-matrix` too.

You could run e.g.:
```sh
cabal-matrix -j1 text --package text --times --package bytestring
```
to try every `text` version against every `bytestring` version. However if
you're running this from your project folder, then the constraints coming from
your project's packages will prevent some versions from being available,
possibly masking some `build fail`s as `no plan`s.

Instead you can ask `cabal-matrix` to create a blank project for building `text`
using `--blank-project`:
```sh
cabal-matrix -j1 --blank-project text \
  --package text --times --package bytestring
```

For a typical yet concrete example, we can time travel to the past using
`--option --index-state=2025-10-01T00:00:00Z`, and try building `tar` against
`directory`:
```sh
cabal-matrix -j1 --blank-project tar \
  --option --index-state=2025-10-01T00:00:00Z \
  --compiler ghc-9.2.8 \
  --times \
  --package tar=">=0.6" \
  --times \
  --package directory=">=1.3"
```

There is a clear cut corner of `build fail`s at `tar >=0.6.4` and
`directory <1.3.8`:
```
 COMPILER│ghc-9.2.8  ghc-9.2.8  ghc-9.2.8  ghc-9.2.8  ghc-9.2.8  ghc-9.2.8
 tar     │0.6.0.0    0.6.1.0    0.6.2.0    0.6.3.0    0.6.4.0    0.7.0.0
─────────┼──────────────────────────────────────────────────────────────────────
directory│
─────────┼──────────────────────────────────────────────────────────────────────
1.3.5.0  ▲no plan    no plan    no plan    no plan    no plan    no plan
1.3.6.0  │no plan    no plan    no plan    no plan    no plan    no plan
1.3.6.1  │no plan    no plan    no plan    no plan    no plan    no plan
1.3.6.2  │build ok   build ok   build ok   build ok   build fail build fail
1.3.7.0  │build ok   build ok   build ok   build ok   build fail build fail
1.3.7.1  │build ok   build ok   build ok   build ok   build fail build fail
1.3.8.0  │build ok   build ok   build ok   build ok   build ok   build ok
1.3.8.1  │build ok   build ok   build ok   build ok   build ok   build ok
1.3.8.2  ▼build ok   build ok   build ok   build ok   build ok   build ok
<Esc>/Q: quit │ X: axes │ ◀▲▼▶: select cell │ <Tab>: focus cells/cols/rows
```
Using arrow keys to navigate to each `build fail` and `<Space>` to view the
output, we see that they are all caused by the same issue of importing
`System.Directory.OsPath`. Double checking with the documentation we see that
the module was added in `directory-1.3.8.0`, and the fact that `tar` uses it
without declaring `build-depends: directory >=1.3.8` is a mistake.

This specific issue has been already reported in
https://github.com/haskell/tar/issues/103 and fixed.

Note that we chose to use a sufficiently old compiler to cover a wide range of
`directory` versions. If we had used GHC 9.8.4, we would have noticed a
`build fail` but our investigation would have been complicated by the fact that
`directory-1.3.8.0` is not buildable, and it's not immediately clear whether the
fix is `directory >=1.3.8` or `directory >=1.3.8.1`. If we had used GHC 9.12.2,
we would not have noticed the problem at all, as `directory <1.3.8` is not
buildable.

In general, some fiddling with GHC versions may be required, even mixing
different GHC versions in the same build, e.g.:
```
--[ --compiler ghc-7.10.3 --times --package directory="<1.3.8" --] \
--add \
--[ --compiler ghc-9.6.7 --times --package directory=">=1.3.8" --]
```

# Loosening Dependency Bounds

If you have dependency bounds, but you're unsure if they're too tight and
can/should be relaxed, you can forward (using `--option`) `--allow-newer` and
`--allow-older` to cabal:
```sh
cabal-matrix -j1 foo --package bytestring \
  --option --allow-newer=foo:bytestring --option --allow-older=foo:bytestring
```
This will tell cabal to ignore the `build-depends` constraints that your package
`foo` has declared on `bytestring`.

With these options, some configurations that were previously `no plan`s may
become `build fail`s, meaning the constraint was correct in excluding those.
Other configurations may become `build ok`, meaning the constraint could be
relaxed to include those. Finally, some `no plan`s may remain as such, meaning
the configuration was excluded for some other reason. You can read the `no plan`
output to find out in more detail.

# Validating Hackage Revisions

If you have a hackage revision that you would like to apply, you can see how it
would affect the build plans by using a `--constraints` option with an
implication constraint. If as in the above example, we would like to revise
`tar >=0.6.4.0 && <=0.7.0.0` to include tighten the bound on `directory` to
`>=1.3.8`, we can do so with:
```sh
cabal-matrix -j1 --blank-project tar \
  --option --index-state=2025-10-01T00:00:00Z \
  --compiler ghc-9.2.8 \
  --times \
  --package tar=">=0.6" \
  --times \
  --package directory=">=1.3" \
  --times \
  --constraints "tar >=0.6.4.0 && <=0.7.0.0 :- directory >=1.3.8"
```

Note that `--constraints` acts as a singleton list, so it needs to be
`--times`'ed with the rest of the build matrix. This allows, if necessary, to
apply different fixes to different parts of the build matrix.

This implication constraint will work even when we're not directly enumerating
every `tar` or `directory` version, and even when neither of them is our build
target. So if you're trying to build your package `foo`, and encouter a
`deps fail` because of e.g. `tar`/`directory`, after figuring out which versions
of `tar` and `directory` are conflicting, you can get back to building your
`foo` package by adding the same
`--constraints "tar >=0.6.4.0 && <=0.7.0.0 :- directory >=1.3.8"` option to your
build.

Note that there's currently no easy way to preview a revision that *relaxes* a
constraint.
