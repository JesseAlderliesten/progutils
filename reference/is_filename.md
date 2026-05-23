# Check that `x` is a valid filename

Check that `x` is a filename that includes an extension and is likely to
be valid.

## Usage

``` r
is_filename(filename)
```

## Arguments

- filename:

  [character
  string](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
  with the filename including the file extension.

## Value

`TRUE`: an error occurs if `filename` is not a valid filename.

## Details

`is_filename()` puts some restrictions on filenames so it can be used to
check for valid filenames before creating a file:

- `filename` should include a file extension. The compression extensions
  `".gz"`, `".bz2"` or `".xz"` are **not** considered file extensions
  but are allowed to be present.

- `filename` should **not** contain characters `"`, `*`, `/`, `:`, `?`,
  `\`, `|`, `<`, `>`, nor any of the control characters (`ASCII` octal
  codes 000 through 037 and 177, see
  [`help("regex")`](https://rdrr.io/r/base/regex.html)).

- `filename` should **not** start with a space.

- The part of `filename` before the extension should **not** end with a
  space nor with a period.

These restrictions consider characters that would lead to an error in
Windows because they are not allowed; characters that would lead to a
mismatch between the created directory and the returned path because
they are silently removed on Windows; and words that are reserved names
in Windows.

In contrast to functions from `checkinput`, `is_filename` will produce
an error if `filename` is not a valid filename.

## Programming notes

Although not enforced by `is_filename()`, it is good practice to also
avoid the characters `+`, `,`, `;`, `=`, `[`, `]`, `!`, `$`, `#`, `@`,
and possibly `{`, `}`, `(`, `)`, `'`, `%`, `&`, , `^`, `~`.

Ways to make `is_filename()` even stricter:

- do not allow filenames to start with a hyphen

- do not allow filenames to end with a hyphen

- case-insensitive matching to `filename` to determine if it exists?
  `filename` should **not** point to a directory (see
  [`utils::file_test()`](https://rdrr.io/r/utils/filetest.html),
  [`get_filename()`](https://jessealderliesten.github.io/progutils/reference/get_filename.md),
  [`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md),
  [`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md)).

- Impose a limit on the length:
  https://blog.r-project.org/2023/03/07/path-length-limit-on-windows/

- See also functions
  [`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md),
  `make_filename()` and
  [`fs::path_real()`](https://fs.r-lib.org/reference/path_math.html).

## References

- https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file

- https://en.wikipedia.org/wiki/Comparison_of_file_systems#Limits

## See also

On file existence and permissions, see
[`utils::file_test()`](https://rdrr.io/r/utils/filetest.html) and
references there and
[`fs::path_real()`](https://fs.r-lib.org/reference/path_math.html).

Other functions to handle paths and directories:
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md),
[`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md),
[`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md),
[`file_path_no_ext()`](https://jessealderliesten.github.io/progutils/reference/file_path_no_ext.md),
[`get_filename()`](https://jessealderliesten.github.io/progutils/reference/get_filename.md),
[`is_path()`](https://jessealderliesten.github.io/progutils/reference/is_path.md)

## Examples

``` r
is_filename("abcd.txt")
#> [1] TRUE
is_filename("abcd.txt.gz")
#> [1] TRUE

try(is_filename("abcd"))
#> Error in is_filename("abcd") : Empty filename or missing extension:
#> abcd
try(is_filename("abcd.gz"))
#> Error in is_filename("abcd.gz") : Empty filename or missing extension:
#> abcd.gz
try(is_filename("ab|cd.txt"))
#> Error in is_filename("ab|cd.txt") : 
#>   'filename' should not contain '"', '*', '/', ':', '?', '', '|', '<' or '>':
#> ab|cd.txt
```
