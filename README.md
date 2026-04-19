
<!-- badges: start -->

![](https://img.shields.io/github/r-package/v/JesseAlderliesten/progutils?color=blue)
[![R-CMD-check](https://github.com/JesseAlderliesten/progutils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JesseAlderliesten/progutils/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# progutils

`progutils` contains utility functions, for example to reorder values,
check equality, construct messages, or handle files and directories.

## Folder structure

    ├── .github
    │   └── workflows: workflows to run tests with GitHub Actions
    ├── R: functions
    ├── inst
    │   └── tinytest: tests
    ├── man: help-files
    └── tests: setup to use 'tinytest' for testing

## Installation

You can install `progutils` from
[GitHub](https://github.com/JesseAlderliesten/progutils) with:

``` r
if(!requireNamespace("remotes", quietly = TRUE)) {
  install.packages(pkgs = "remotes", quiet = FALSE)
}
remotes::install_github(repo = "JesseAlderliesten/progutils", dependencies = TRUE,
                        upgrade = FALSE, force = FALSE, quiet = FALSE,
                        build_vignettes = TRUE, lib = NULL,
                        verbose = getOption("verbose"))
```

For information about installing and configuring R and RStudio, see my
repository
[checkrpkgs](https://github.com/JesseAlderliesten/checkrpkgs).

## Examples

Every once in a while, reading data into R results in a factor that
should be converted to a numeric vector. The code below illustrates that
using `as.numeric(x)` does *not* work to achieve this while
`as.numeric_safe()` *does* work.

``` r
library(progutils)
x <- factor(11:13)
x
#> [1] 11 12 13
#> Levels: 11 12 13
as.numeric(x) # Returns the indices instead of the values!
#> [1] 1 2 3
as.numeric_safe(x) # Returns the values.
#> [1] 11 12 13
```

Using `relevel` or `levels()` to reorder factor levels does *not* work,
whereas function `reorder_levels()` *does*:

``` r
library(progutils)
f_orig <- factor(letters[c(12:13, 13:11)], levels = letters[13:11])
f_orig
#> [1] l m m l k
#> Levels: m l k

f_test <- f_orig
levels(f_test) <- letters[11:13]
f_test # Also changes the values!
#> [1] l k k l m
#> Levels: k l m
reorder_levels(x = f_orig, new_order = letters[11:13]) # Only changes the levels
#> [1] l m m l k
#> Levels: k l m
```

Using a named numeric vector in the subtitle of a plot requires
converting it a to a character string wrapped to an appropriate length,
to prevent text from running of the plot. Combining `vect_to_char()` and
`wrap_text()` provides a way to achieve that:

``` r
library(progutils)
msg_part_one <- "Some\nmessage you want to display"
num_vect <- c(j = 10, k = 11, l = 12)

cat(wrap_text(paste(msg_part_one, vect_to_char(num_vect)), width = 20))
#> Some message you
#> want to display j:
#> 10, k: 11, l: 12

# Retaining newlines that were present in the original message:
cat(wrap_text(paste(msg_part_one, vect_to_char(num_vect)), width = 20,
              ignore_newlines = FALSE))
#> Some
#> message you want to
#> display j: 10, k:
#> 11, l: 12
```

# Alternatives

Many packages with miscellaneous functions exist on
[GitHub](https://github.com/) and
[CRAN](https://cran.r-project.org/web/packages/available_packages_by_name.html):
frequently they have ‘misc’ or ‘util’ in their title or description. In
addition, many packages have an `utils` folder in `R`.
