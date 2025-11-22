# Next

- Binary operator options can now be parsed/formatted as JSON in a more compact
  way: `foo --times bar --times baz` is can now be serialized/deserialized as
  `{"times":[foo,bar,baz]}`
- `--package PACKAGE` (i.e. without any version or version range specified) now
  stands for all versions of `PACKAGE`. Likewise in JSON
  `{"package":"PACKAGE","versions":null}` now stands for all versions.

# 1.0.0.0

Initial Hackage release
