# Check if only a single matching file is present

Check if only one file in directory `dir` has a name matching `pattern`,
for example before attempting to [read a
file](https://rdrr.io/r/utils/read.table.html).

## Usage

``` r
get_filename(dir = ".", pattern, ignore_case = TRUE, quietly = FALSE)
```

## Arguments

- dir:

  Character string with the
  [path](https://rdrr.io/r/base/file.path.html) to a directory.

- pattern:

  Character string containing a [regular
  expression](https://rdrr.io/r/base/regex.html) used to select file
  names in `dir`.

- ignore_case:

  `TRUE` or `FALSE`: use case-insensitive pattern matching?

- quietly:

  `TRUE` or `FALSE`: suppress the message with the found file name?

## Value

A character string with the file name matching `pattern` if there is
exactly one such file in directory `dir`. Otherwise an error is thrown.

## Details

The default `"."` for `dir` indicates the [working
directory](https://rdrr.io/r/base/getwd.html).

If `ignore_case` is `FALSE` and no case-sensitive match is found, the
error message indicates if any case-insensitive match is present.

In contrast to the default of
[`list.files()`](https://rdrr.io/r/base/list.files.html),
`get_filename()` also finds 'hidden' files, i.e., files with names that
start with a dot.

Paths will be [normalized](https://rdrr.io/r/base/normalizePath.html)
before use, to ensure they still work if the [working
directory](https://rdrr.io/r/base/getwd.html) changes. `"/"` instead of
`"\\"` is used as argument
[winslash](https://rdrr.io/r/base/normalizePath.html) such that the
returned path can be used in Windows' file system.

## See also

[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md)
to create a directory if does not yet exist,
[`file.exists()`](https://rdrr.io/r/base/files.html) and
[`list.files()`](https://rdrr.io/r/base/list.files.html) to check for
existence of files without checking they are a unique match to a
pattern, [`file.info()`](https://rdrr.io/r/base/file.info.html) and
[`file.access()`](https://rdrr.io/r/base/file.access.html) to extract
information about files or directories;
[`file.path()`](https://rdrr.io/r/base/file.path.html) to construct file
paths in a platform-independent way;
[`normalizePath()`](https://rdrr.io/r/base/normalizePath.html) to create
absolute paths.

Other functions to handle paths and directories:
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md),
[`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md),
[`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md),
[`file_path_no_ext()`](https://jessealderliesten.github.io/progutils/reference/file_path_no_ext.md),
[`is_filename()`](https://jessealderliesten.github.io/progutils/reference/is_filename.md),
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

get_filename(dir = tempdir(), pattern = "some_file")
#> Using file 'some_filename1a004e0e8e67.txt'
#> [1] "some_filename1a004e0e8e67.txt"

# The same file is found if case-insensitive matching is used:
get_filename(dir = tempdir(), pattern = "SOME_FILE", ignore_case = TRUE)
#> Using file 'some_filename1a004e0e8e67.txt'
#> [1] "some_filename1a004e0e8e67.txt"

# Error reporting presence of case-insensitive match.
try(get_filename(dir = tempdir(), pattern = "SOME_FILE", ignore_case = FALSE))
#> Error in get_filename(dir = tempdir(), pattern = "SOME_FILE", ignore_case = FALSE) : 
#>   No case-sensitive matches to pattern 'SOME_FILE' are present in directory
#> '/tmp/RtmpoAfuMi'.
#> However, a case-insensitive match to 'pattern' is present: 'some_filename1a004e0e8e67.txt'.

# Error reporting no match found.
try(get_filename(dir = tempdir(), pattern = "missing_filename_abcde",
                 ignore_case = TRUE))
#> Error in get_filename(dir = tempdir(), pattern = "missing_filename_abcde",  : 
#>   No case-insensitive matches to pattern 'missing_filename_abcde' are present in directory
#> '/tmp/RtmpoAfuMi'.
try(get_filename(dir = tempdir(), pattern = "missing_filename_abcde",
                 ignore_case = FALSE))
#> Error in get_filename(dir = tempdir(), pattern = "missing_filename_abcde",  : 
#>   No case-sensitive matches to pattern 'missing_filename_abcde' are present in directory
#> '/tmp/RtmpoAfuMi'.
#> No case-insensitive match is present either.

# Error if multiple matches are present.
try(get_filename(dir = tempdir(), pattern = "_filename"))
#> Error in get_filename(dir = tempdir(), pattern = "_filename") : 
#>   Multiple case-insensitive matches to pattern '_filename' are present in directory
#> '/tmp/RtmpoAfuMi': 'another_filename1a00720a1021.txt', 'some_filename1a004e0e8e67.txt'!

# Clean up
unlink(x = my_tempfiles)
rm(my_tempfiles)
```
