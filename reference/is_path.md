# Check that `x` is a valid path

Check that `x` is a path that is likely to be valid.

## Usage

``` r
is_path(path, as_file = FALSE)
```

## Arguments

- path:

  [character
  string](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
  with the path.

- as_file:

  `TRUE` or `FALSE`: is `path` intended to point to a file instead of to
  a folder?

## Value

`TRUE`: an error occurs if `path` is not a valid path.

## Details

`is_path()` puts some restrictions on paths so it can be used to check
for valid paths before creating a directory:

- `path` should **not** contain the characters `"`, `*`, `?`, `|`, `<`,
  `>`, nor any of the control characters (`ASCII` octal codes 000
  through 037 and 177, see
  [`help("regex")`](https://rdrr.io/r/base/regex.html)).

- `path` components should **not** end with a slash, backslash, space or
  dot (`"."` and `".."` are allowed as first component to indicate the
  working directory and the parent directory, respectively).

- `path` components should **not** be `CON`, `PRN`, `AUX`, `NUL`,
  `COM<non-zero digit>`, `LPT<non-zero digit>`, case-insensitive
  variants of these names, and these names followed by an extension.

- `path` should not point to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html): a temporary
  subdirectory should be used instead (see
  [`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md)).

These restrictions consider characters that would lead to an error in
Windows because they are not allowed; characters that would lead to a
mismatch between the created directory and the returned path because
they are silently removed in Windows; and words that are reserved names
in Windows.

In contrast to functions from `checkinput`, `is_path` will produce an
error if `path` is not a valid path.

## Programming notes

On MacOS, the output of
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) is preceded by
duplicated forward slashes in R cmd checks (e.g.,
`/var/[...]/T//RtmpxC2Fyl/working_dir/RtmpdnqgUR`), leading to a
spurious warning from `is_path()`.

The file separator is a backslash (`\`) on Windows but a forward slash
(`/`) on other operating systems
([.Platform\$file.sep](https://rdrr.io/r/base/Platform.html) gives the
file separator used on the current platform). Furthermore, the backslash
is used as [escape character](https://rdrr.io/r/base/regex.html) in R,
such that backslashes need to be escaped in R code. Thus, to warn if a
[string](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
contains a file separator, one should write the warning message as
`warning("Repeated '/' or '\\'")` which will be printed as
`Repeated '/' or '\'`. Checks on the presence of slashes and backslashes
should use `grepl(pattern = "/", x = string)` and
`grepl(pattern = "\\\\", x = string)` (!). This makes it cumbersome to
get the correct type and number of slashes to compare with the path
recorded in a warning message, such that it is more robust to check only
for fixed parts of the message (e.g., `"Repeated"`), possibly followed
by a check like `tinytest::expect_true(dir.exists(string))`.

## References

- https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file

- https://en.wikipedia.org/wiki/Comparison_of_file_systems#Limits

## See also

[`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md)
to create a path (with references there about file paths),
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md)
to create a directory if it does not yet exist,
[`get_filename()`](https://jessealderliesten.github.io/progutils/reference/get_filename.md)
to check if a file exists and is a unique match to a pattern

[`fs::path_sanitize()`](https://fs.r-lib.org/reference/path_sanitize.html)
to *remove* invalid characters from potential paths.

Other functions to handle paths and directories:
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md),
[`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md),
[`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md),
[`file_path_no_ext()`](https://jessealderliesten.github.io/progutils/reference/file_path_no_ext.md),
[`get_filename()`](https://jessealderliesten.github.io/progutils/reference/get_filename.md),
[`is_filename()`](https://jessealderliesten.github.io/progutils/reference/is_filename.md)

## Examples

``` r
is_path(getwd())
#> [1] TRUE
try(is_path(file.path(getwd(), "ab|cd")))
#> Error in is_path(file.path(getwd(), "ab|cd")) : 
#>   'file.path(getwd(), "ab|cd")' should not contain '"', '*', '?', '|', '<' or '>':
#> /home/runner/work/progutils/progutils/docs/reference/ab|cd
```
