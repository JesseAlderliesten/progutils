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

  Character string giving the
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
`get_filename()` also finds 'hidden' files, i.e., files which names
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
[`create_path()`](https://jessealderliesten.github.io/progutils/reference/create_path.md),
[`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md),
[`file_path_sans_ext()`](https://jessealderliesten.github.io/progutils/reference/file_path_sans_ext.md)

Other functions to check equality:
[`are_equal()`](https://jessealderliesten.github.io/progutils/reference/are_equal.md),
[`check_case()`](https://jessealderliesten.github.io/progutils/reference/check_case.md),
[`not_in()`](https://jessealderliesten.github.io/progutils/reference/not_in.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md)

## Examples

``` r
# Create files in a temporary directory so we know what is present.
my_tempfiles <- tempfile(pattern = c("FirstFile", "SecondFile"), fileext = ".txt")
# Create the files
file.create(my_tempfiles)
#> [1] TRUE TRUE

get_filename(dir = tempdir(), pattern = "First")
#> Using file 'FirstFile1a794db8866b.txt'
#> [1] "FirstFile1a794db8866b.txt"
# The same file is found if case-insensitive matching is used:
get_filename(dir = tempdir(), pattern = "FIRST", ignore_case = TRUE)
#> Using file 'FirstFile1a794db8866b.txt'
#> [1] "FirstFile1a794db8866b.txt"
# Error reporting presence of case-insensitive match.
try(get_filename(dir = tempdir(), pattern = "FIRST", ignore_case = FALSE))
#> Error in get_filename(dir = tempdir(), pattern = "FIRST", ignore_case = FALSE) : 
#>   No case-sensitive matches to pattern 'FIRST' are present in directory
#> '/tmp/RtmpU9aiax'.
#> However, a case-insensitive match to 'pattern' is present: 'FirstFile1a794db8866b.txt'.
# Error reporting no match found.
try(get_filename(dir = tempdir(), pattern = "abcde", ignore_case = TRUE))
#> Error in get_filename(dir = tempdir(), pattern = "abcde", ignore_case = TRUE) : 
#>   No case-insensitive matches to pattern 'abcde' are present in directory
#> '/tmp/RtmpU9aiax'.
try(get_filename(dir = tempdir(), pattern = "abcde", ignore_case = FALSE))
#> Error in get_filename(dir = tempdir(), pattern = "abcde", ignore_case = FALSE) : 
#>   No case-sensitive matches to pattern 'abcde' are present in directory
#> '/tmp/RtmpU9aiax'.
#> No case-insensitive match is present either.
# Error because multiple matches are present.
try(get_filename(dir = tempdir(), pattern = "File"))
#> Error in get_filename(dir = tempdir(), pattern = "File") : 
#>   Multiple case-insensitive matches to pattern 'File' are present in directory
#> '/tmp/RtmpU9aiax': 'FirstFile1a794db8866b.txt', 'SecondFile1a79668c4408.txt', 'file1a796ef39a0d', 'file1a7977af8022'!

# Deleting the created temporary files
unlink(x = my_tempfiles)
```
