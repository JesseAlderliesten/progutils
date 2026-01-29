
# progutils

`progutils` contains utility functions I frequently use: functions to
check equality, functions to construct messages about an input vector,
and functions to handle files and directories.

## Installation

You can install `progutils` from
[GitHub](https://github.com/JesseAlderliesten/progutils) with:

``` r
if(!requireNamespace("remotes")) {
  install.packages("remotes")
}
remotes::install_github("JesseAlderliesten/progutils")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(progutils)
## basic example code
```

# Alternatives

Many packages with miscellaneous functions exist on
[GitHub](https://github.com/) and
[CRAN](https://cran.r-project.org/web/packages/available_packages_by_name.html):
frequently they have ‘misc’ or ‘util’ in the title or description. In
addition, many packages have an ‘utils’ folder in ‘R/’.

Some notable collections:

- [xfun::is_ascii](https://github.com/yihui/xfun) by Yihui Xie
