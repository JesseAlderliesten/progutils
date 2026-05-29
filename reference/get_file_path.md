# Check that only a single matching file is present

Check that only one file in a directory has a name matching `pattern`,
for example before attempting to [read a
file](https://rdrr.io/r/utils/read.table.html).

## Usage

``` r
get_file_path(dir = ".", pattern, ignore_case = TRUE, quietly = FALSE)
```

## Arguments

- dir:

  Character string with the
  [path](https://jessealderliesten.github.io/progutils/reference/is_path.md)
  to a directory.

- pattern:

  Character string containing a [regular
  expression](https://rdrr.io/r/base/regex.html) used to select names of
  files that are present in `dir`.

- ignore_case:

  `TRUE` or `FALSE`: use case-insensitive pattern matching?

- quietly:

  `TRUE` or `FALSE`: suppress the message with the found file name?

## Value

A character string with the
[normalized](https://rdrr.io/r/base/normalizePath.html) path to the file
with a name matching `pattern` if there is exactly one such file in
directory `dir`. Otherwise an error is thrown. Use
[`basename()`](https://rdrr.io/r/base/basename.html) on the result to
obtain the filename itself.

## Details

The default `dir` (`"."`) indicates the [working
directory](https://rdrr.io/r/base/getwd.html).

If `ignore_case` is `FALSE` and no case-sensitive match is found, the
error message indicates if any case-insensitive match is present.

In contrast to the default of
[`list.files()`](https://rdrr.io/r/base/list.files.html),
`get_file_path()` also finds 'hidden' files, i.e., files with names that
start with a dot.

Paths will be [normalized](https://rdrr.io/r/base/normalizePath.html) to
ensure they still work if the [working
directory](https://rdrr.io/r/base/getwd.html) changes. `"/"` instead of
`"\\"` is used as argument
[winslash](https://rdrr.io/r/base/normalizePath.html) such that the
returned path can be used in Windows' file system.

## See also

[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md)
to create a directory if does not yet exist;
[`file.exists()`](https://rdrr.io/r/base/files.html) and
[`list.files()`](https://rdrr.io/r/base/list.files.html) to check for
existence of files without checking they are a unique match to a
pattern; [`file.info()`](https://rdrr.io/r/base/file.info.html) and
[`file.access()`](https://rdrr.io/r/base/file.access.html) to extract
information about files or directories;
[`fs::path()`](https://fs.r-lib.org/reference/path.html) to construct
file paths in a platform-independent way;
[`normalizePath()`](https://rdrr.io/r/base/normalizePath.html) to create
absolute paths.

Other functions to handle paths and directories:
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md),
[`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md),
[`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md),
[`is_path()`](https://jessealderliesten.github.io/progutils/reference/is_path.md)

Other functions to check equality:
[`are_equal()`](https://jessealderliesten.github.io/progutils/reference/are_equal.md),
[`check_case()`](https://jessealderliesten.github.io/progutils/reference/check_case.md),
[`not_in()`](https://jessealderliesten.github.io/progutils/reference/not_in.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md)

## Examples

``` r
# Create files in a temporary directory so we know what is present.
my_tempfiles <- tempfile(pattern = c("some_filename", "another_filename"),
                         fileext = ".txt")
# Create the files
file.create(my_tempfiles)
#> [1] TRUE TRUE

get_file_path(dir = tempdir(), pattern = "some_file")
#> Using file '/tmp/RtmpR0HFYq/some_filename19647125db0a.txt'
#> [1] "/tmp/RtmpR0HFYq/some_filename19647125db0a.txt"

# The same file is found if case-insensitive matching is used:
get_file_path(dir = tempdir(), pattern = "SOME_FILE", ignore_case = TRUE)
#> Using file '/tmp/RtmpR0HFYq/some_filename19647125db0a.txt'
#> [1] "/tmp/RtmpR0HFYq/some_filename19647125db0a.txt"

# Error reporting the presence of a case-insensitive match.
try(get_file_path(dir = tempdir(), pattern = "SOME_FILE", ignore_case = FALSE))
#> Error in get_file_path(dir = tempdir(), pattern = "SOME_FILE", ignore_case = FALSE) : 
#>   No case-sensitive matches to pattern 'SOME_FILE' are present in directory
#> '/tmp/RtmpR0HFYq'.
#> However, a case-insensitive match to 'pattern' is present: 'some_filename19647125db0a.txt'.

# Error reporting no match found.
try(get_file_path(dir = tempdir(), pattern = "missing_filename_abcde",
                 ignore_case = TRUE))
#> Error in get_file_path(dir = tempdir(), pattern = "missing_filename_abcde",  : 
#>   No matches to pattern 'missing_filename_abcde' are present in directory
#> '/tmp/RtmpR0HFYq'.
try(get_file_path(dir = tempdir(), pattern = "missing_filename_abcde",
                 ignore_case = FALSE))
#> Error in get_file_path(dir = tempdir(), pattern = "missing_filename_abcde",  : 
#>   No case-sensitive matches to pattern 'missing_filename_abcde' are present in directory
#> '/tmp/RtmpR0HFYq'.
#> No case-insensitive match is present either.

# Error if multiple matches are present.
try(get_file_path(dir = tempdir(), pattern = "_filename"))
#> Error in get_file_path(dir = tempdir(), pattern = "_filename") : 
#>   Multiple matches to pattern '_filename' are present in directory
#> '/tmp/RtmpR0HFYq': 'another_filename1964327e8506.txt', 'some_filename19647125db0a.txt'!

# Clean up
unlink(x = my_tempfiles)
rm(my_tempfiles)
```
