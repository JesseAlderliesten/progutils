# Get the file path to a matching file

Get the file path to a matching file while ensuring that only one file
in a directory has a name matching `pattern`, for example before
attempting to [read a file](https://rdrr.io/r/utils/read.table.html).

## Usage

``` r
get_file_path(dir = ".", pattern, ignore_case = TRUE, quietly = FALSE)
```

## Arguments

- dir:

  [character
  string](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
  containing a [valid
  path](https://jessealderliesten.github.io/checkinput/reference/is_path.html),
  see `Details` and the section `Notes on paths` below.

- pattern:

  [character
  string](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
  containing a [regular expression](https://rdrr.io/r/base/regex.html)
  used to select names of files that are present in `dir`.

- ignore_case:

  `TRUE` or `FALSE`: use
  [case-insensitive](https://rdrr.io/r/base/chartr.html) pattern
  matching?

- quietly:

  `TRUE` or `FALSE`: suppress the message with the found file name?

## Value

A character string with the [absolute
normalized](https://fs.r-lib.org/reference/path_math.html) path to the
file with a name matching `pattern` if there is exactly one such file in
directory `dir`. Use
[`basename()`](https://rdrr.io/r/base/basename.html) on the result to
obtain the filename itself.

An error is thrown if no match, or multiple matches, are found. If
`ignore_case` is `FALSE`, the error message indicates if any
case-insensitive match is present.

## Details

The path in `dir` is interpreted as a **relative** path to the directory
where to look for a file, starting from the [working
directory](https://rdrr.io/r/base/getwd.html), see the section
`Notes on paths` below.

In contrast to the default of
[`list.files()`](https://rdrr.io/r/base/list.files.html),
`get_file_path()` includes 'hidden' files, i.e., files with names that
start with a dot, but excludes directories.

## Notes on paths

The file separator is a backslash (`\`) on Windows but a forward slash
(`/`) on other operating systems: use
[`file.path()`](https://rdrr.io/r/base/file.path.html) or
[`fs::path()`](https://fs.r-lib.org/reference/path.html) to construct
paths in a platform-independent way and
[.Platform\$file.sep](https://rdrr.io/r/base/Platform.html) to get the
file separator used on the current platform.

Furthermore, the backslash is also used as [escape
character](https://rdrr.io/r/base/regex.html) in R, such that
backslashes need to be escaped in R code by doubling them. Use `cat(x)`
to omit the escape characters to see how `x` would be printed.

The [working directory](https://rdrr.io/r/base/getwd.html) can be
indicated by a single dot (`"."`) and the parent directory can be
indicated by two dots (`".."`).

**relative** paths start from the [working
directory](https://rdrr.io/r/base/getwd.html). In contrast, **absolute**
paths start from the root directory and thus give the complete path,
such that the path still points to the same directory if the [working
directory](https://rdrr.io/r/base/getwd.html) changes.

Paths can be normalised, e.g., through
[`normalizePath()`](https://rdrr.io/r/base/normalizePath.html) or
[`fs::path_norm()`](https://fs.r-lib.org/reference/path_math.html)
(which is also called by
[`fs::path_abs()`](https://fs.r-lib.org/reference/path_math.html)). On
case-insensitive file systems, this also adjusts the case to match names
of directories that are already present.

## See also

[`checkinput::is_path()`](https://jessealderliesten.github.io/checkinput/reference/is_path.html)
to check if a path is valid, with extensive references about file paths
and directories;
[`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md)
to create a file path and creating the indicated directory if it does
not yet exist;
[`fs::file_exists()`](https://fs.r-lib.org/reference/file_access.html)
and [`list.files()`](https://rdrr.io/r/base/list.files.html) (which
**includes** directories in its output) to check for existence of files
without checking they are a unique match to a pattern;
[`file.info()`](https://rdrr.io/r/base/file.info.html) and
[`file.access()`](https://rdrr.io/r/base/file.access.html) to extract
information about files or directories

Other functions to handle paths and directories:
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md),
[`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md),
[`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md)

Other functions to check equality:
[`are_equal()`](https://jessealderliesten.github.io/progutils/reference/are_equal.md),
[`check_case()`](https://jessealderliesten.github.io/progutils/reference/check_case.md),
[`not_in()`](https://jessealderliesten.github.io/progutils/reference/not_in.md),
[`replace_nonalnum()`](https://jessealderliesten.github.io/progutils/reference/replace_nonalnum.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md),
[`signif_custom()`](https://jessealderliesten.github.io/progutils/reference/signif_custom.md)

## Examples

``` r
# Create files in a temporary directory so we know what is present.
my_tempdir <- create_tempdir(prefix = "examplegetfilepath")
my_tempfiles <- fs::path_abs(
  fs::path(my_tempdir, paste0(c("some_filename", "another_filename"), ".txt"))
)

# Create the files
file.create(my_tempfiles)
#> [1] TRUE TRUE

get_file_path(dir = my_tempdir, pattern = "some_file")
#> Using file '/tmp/RtmpErmCfJ/examplegetfilepath187d651cf444/some_filename.txt'
#> /tmp/RtmpErmCfJ/examplegetfilepath187d651cf444/some_filename.txt

# The same file is found if case-insensitive matching is used:
get_file_path(dir = my_tempdir, pattern = "SOME_FILE", ignore_case = TRUE)
#> Using file '/tmp/RtmpErmCfJ/examplegetfilepath187d651cf444/some_filename.txt'
#> /tmp/RtmpErmCfJ/examplegetfilepath187d651cf444/some_filename.txt

# Error reporting the presence of a case-insensitive match.
try(get_file_path(dir = my_tempdir, pattern = "SOME_FILE", ignore_case = FALSE))
#> Error in get_file_path(dir = my_tempdir, pattern = "SOME_FILE", ignore_case = FALSE) : 
#>   No case-sensitive matches to pattern 'SOME_FILE' are present in directory
#> '/tmp/RtmpErmCfJ/examplegetfilepath187d651cf444'.
#> However, one or more case-insensitive matches to 'pattern' are present:
#> 'some_filename.txt'.

# 'pattern' is interpreted as a regular expression
get_file_path(dir = my_tempdir, pattern = "^.+er_file")
#> Using file '/tmp/RtmpErmCfJ/examplegetfilepath187d651cf444/another_filename.txt'
#> /tmp/RtmpErmCfJ/examplegetfilepath187d651cf444/another_filename.txt

# Error reporting no match found.
try(get_file_path(dir = my_tempdir, pattern = "missing_filename_abcde",
                 ignore_case = TRUE))
#> Error in get_file_path(dir = my_tempdir, pattern = "missing_filename_abcde",  : 
#>   No matches to pattern 'missing_filename_abcde' are present in directory
#> '/tmp/RtmpErmCfJ/examplegetfilepath187d651cf444'.
try(get_file_path(dir = my_tempdir, pattern = "missing_filename_abcde",
                 ignore_case = FALSE))
#> Error in get_file_path(dir = my_tempdir, pattern = "missing_filename_abcde",  : 
#>   No case-sensitive matches to pattern 'missing_filename_abcde' are present in directory
#> '/tmp/RtmpErmCfJ/examplegetfilepath187d651cf444'.
#> No case-insensitive match is present either.

# Error if multiple matches are present.
try(get_file_path(dir = my_tempdir, pattern = "_filename"))
#> Error in get_file_path(dir = my_tempdir, pattern = "_filename") : 
#>   Multiple matches to pattern '_filename' are present in directory
#> '/tmp/RtmpErmCfJ/examplegetfilepath187d651cf444':
#> 'another_filename.txt'
#> 'some_filename.txt'!

# Clean up
unlink(x = my_tempfiles)
rm(my_tempfiles)
```
