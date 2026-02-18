
![](https://img.shields.io/github/r-package/v/JesseAlderliesten/progutils)

# progutils

`progutils` contains utility functions I frequently use: functions to
check equality, functions to construct messages about an input vector,
and functions to handle files and directories.

# Installation

You can install `progutils` from
[GitHub](https://github.com/JesseAlderliesten/progutils) with:

``` r
if(!requireNamespace("remotes", quietly = TRUE)) {
  install.packages(pkgs = "remotes", quiet = FALSE)
}
remotes::install_github(repo = "JesseAlderliesten/progutils", dependencies = TRUE,
                        upgrade = FALSE, force = FALSE, quiet = FALSE,
                        build_vignettes = TRUE, lib = .libPaths(),
                        verbose = getOption("verbose"))
```

For information about installing and configuring R and RStudio, see my
[repository
‘checkrpkgs’](https://github.com/JesseAlderliesten/checkrpkgs).

# Example

This is a basic example which shows you how to solve a common problem:

``` r
library(progutils)
x <- factor(11:13)
x
#> [1] 11 12 13
#> Levels: 11 12 13
as.numeric(x) # c(1L, 2L, 3L): the indices instead of the values!
#> [1] 1 2 3
as.numeric_safe(x) # c(11, 12, 13)
#> [1] 11 12 13

cat(wrap_text("Some\nmessage you want to display", width = 20))
#> Some message you
#> want to display
cat(wrap_text("Some\nmessage you want to display", width = 20,
              ignore_newlines = FALSE))
#> Some
#> message you want to
#> display
```

# Alternatives

Many packages with miscellaneous functions exist on
[GitHub](https://github.com/) and
[CRAN](https://cran.r-project.org/web/packages/available_packages_by_name.html):
frequently they have ‘misc’ or ‘util’ in their title or description. In
addition, many packages have an `utils` folder in `R`.
