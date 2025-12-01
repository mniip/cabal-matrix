# 1.0.2.0

- `--blank-project` is a new mode for building packages directly from hackage,
  and is a much faster alternative to `--install-lib`. It works by creating a
  temporary `cabal.project` file, while contents are simply
  ```
  extra-packages: target1 target2 ...
  ```
  This causes `target1`... to be interpreted as simultaneously a package to be
  downloaded from hackage, and a package to be considered local, i.e. included
  in the "build" step and excluded from the "deps" step.
- `--constraints` is a new option for specifying build constraints. It allows
  specifying one or more constraints on package versions with
  `--constraints "foo >1 , bar <2"`. It also allows specifying implication
  constraints such as `--constraints "foo >1 :- bar <2"` e.g. as if all versions
  `foo >1` were revised to include a `bar <2` constraint. More generally it
  allows specifying disjunction constraints separated with `;`, such as
  `--constraints "foo >1 ; bar <2 ; baz ==3"`. Disjunction/implication
  constraints are translated to constraints guarded behind automatic cabal
  flags.

# 1.0.1.0

- Binary operator options can now be parsed/formatted as JSON in a more compact
  way: `foo --times bar --times baz` can now be serialized/deserialized as
  `{"times":[foo,bar,baz]}`
- `--package PACKAGE` (i.e. without any version or version range specified) now
  stands for all versions of `PACKAGE`. Likewise in JSON
  `{"package":"PACKAGE","versions":null}` now stands for all versions.
- In the TUI, a build can now be:
  - interrupted with Ctrl+C
  - terminated with Ctrl+\
  - prioritized with P
  - restarted with R

# 1.0.0.0

Initial Hackage release
