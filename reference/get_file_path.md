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
  [path](https://jessealderliesten.github.io/checkinput/reference/is_path.html)
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

A character string with the [absolute
normalized](https://fs.r-lib.org/reference/path_math.html) path to the
file with a name matching `pattern` if there is exactly one such file in
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

Paths will be
[normalized](https://fs.r-lib.org/reference/path_math.html) to ensure
they still work if the [working
directory](https://rdrr.io/r/base/getwd.html) changes.

## See also

[`checkinput::is_path()`](https://jessealderliesten.github.io/checkinput/reference/is_path.html)
to check if a path is valid, and the `Note on paths` in its
documentation;
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
[`fs::path_abs()`](https://fs.r-lib.org/reference/path_math.html) to
create absolute paths.

Other functions to handle paths and directories:
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md),
[`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md),
[`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md)

Other functions to check equality:
[`are_equal()`](https://jessealderliesten.github.io/progutils/reference/are_equal.md),
[`check_case()`](https://jessealderliesten.github.io/progutils/reference/check_case.md),
[`not_in()`](https://jessealderliesten.github.io/progutils/reference/not_in.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md)

## Examples

``` r
# Create files in a temporary directory so we know what is present.
my_tempdir <- create_tempdir(pattern = "examplegetfilepath")
my_tempfiles <- fs::path_abs(
  fs::path(my_tempdir, paste0(c("some_filename", "another_filename"), ".txt"))
)

# Create the files
file.create(my_tempfiles)
#> [1] TRUE TRUE

get_file_path(dir = my_tempdir, pattern = "some_file")
#> Using file '/tmp/Rtmp1YgbDN/examplegetfilepath197b101e8bee/some_filename.txt'
#> /tmp/Rtmp1YgbDN/examplegetfilepath197b101e8bee/some_filename.txt

# The same file is found if case-insensitive matching is used:
get_file_path(dir = my_tempdir, pattern = "SOME_FILE", ignore_case = TRUE)
#> Using file '/tmp/Rtmp1YgbDN/examplegetfilepath197b101e8bee/some_filename.txt'
#> /tmp/Rtmp1YgbDN/examplegetfilepath197b101e8bee/some_filename.txt

# Error reporting the presence of a case-insensitive match.
try(get_file_path(dir = my_tempdir, pattern = "SOME_FILE", ignore_case = FALSE))
#> Error in get_file_path(dir = my_tempdir, pattern = "SOME_FILE", ignore_case = FALSE) : 
#>   No case-sensitive matches to pattern 'SOME_FILE' are present in directory
#> '/tmp/Rtmp1YgbDN/examplegetfilepath197b101e8bee'.
#> However, a case-insensitive match to 'pattern' is present: 'some_filename.txt'.

# 'pattern' is interpreted as a regular expression
get_file_path(dir = my_tempdir, pattern = "^.+er_file")
#> Using file '/tmp/Rtmp1YgbDN/examplegetfilepath197b101e8bee/another_filename.txt'
#> /tmp/Rtmp1YgbDN/examplegetfilepath197b101e8bee/another_filename.txt

# Error reporting no match found.
try(get_file_path(dir = my_tempdir, pattern = "missing_filename_abcde",
                 ignore_case = TRUE))
#> Error in get_file_path(dir = my_tempdir, pattern = "missing_filename_abcde",  : 
#>   No matches to pattern 'missing_filename_abcde' are present in directory
#> '/tmp/Rtmp1YgbDN/examplegetfilepath197b101e8bee'.
try(get_file_path(dir = my_tempdir, pattern = "missing_filename_abcde",
                 ignore_case = FALSE))
#> Error in get_file_path(dir = my_tempdir, pattern = "missing_filename_abcde",  : 
#>   No case-sensitive matches to pattern 'missing_filename_abcde' are present in directory
#> '/tmp/Rtmp1YgbDN/examplegetfilepath197b101e8bee'.
#> No case-insensitive match is present either.

# Error if multiple matches are present.
try(get_file_path(dir = my_tempdir, pattern = "_filename"))
#> Error in get_file_path(dir = my_tempdir, pattern = "_filename") : 
#>   Multiple matches to pattern '_filename' are present in directory
#> '/tmp/Rtmp1YgbDN/examplegetfilepath197b101e8bee': 'another_filename.txt', 'some_filename.txt'!

# Clean up
unlink(x = my_tempfiles)
rm(my_tempfiles)
```
