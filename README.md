# glob-intersection

This is a Haskell port of the Go package located here:
<https://github.com/yashtewari/glob-intersection>.

## Usage

1.  Import `Data.GlobIntersection`
2.  Use `parse` to obtain a `Pattern Char` (or an error)
3.  Check if two patterns overlap using `intersects`

## Limitations

It shares the set of limitations with the Go package:

- It is assumed that all input is rooted at the beginning and the end, i.e, starts and ends with the regexp symbols `^` and `$` respectively. This is done because any non-rooted expressions will always match a non-empty set of non-empty strings.
- The only special symbols are:
  - `.` for any character.
  - `+` for 1 or more of the preceding expression.
  - `*` for 0 or more of the preceding expression.
  - `[` and `]` to define regexp-style character classes.
  - `-` to specify Unicode ranges inside character class definitions.
  - `\` escapes any special symbol, including itself.
