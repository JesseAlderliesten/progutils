# Show head and tail of object

Show head and tail of object

## Usage

``` r
head_tail(x, n = 3L)
```

## Arguments

- x:

  object of which the first and last part should be returned.

- n:

  [natural](https://jessealderliesten.github.io/checkinput/reference/is_natural.html)
  number (default `3L`) indicating the number of elements or rows to
  return as `head` and `tail`.

## Value

The `n` first and last elements or rows of `x`.

## See also

[`utils::head()`](https://rdrr.io/r/utils/head.html)
[`utils::tail()`](https://rdrr.io/r/utils/head.html)

## Examples

``` r
x <- letters[1:10]
names(x) <- LETTERS[1:10]
x
#>   A   B   C   D   E   F   G   H   I   J 
#> "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" 
head_tail(x)
#>   A   B   C   H   I   J 
#> "a" "b" "c" "h" "i" "j" 
head_tail(x, n = 4)
#>   A   B   C   D   G   H   I   J 
#> "a" "b" "c" "d" "g" "h" "i" "j" 
head_tail(x, n = 40)
#>   A   B   C   D   E   F   G   H   I   J 
#> "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" 
try(head_tail(x, n = 0))
#> Error in checkinput::make_natural(n, strict = TRUE) : 
#>   checkinput::all_natural(n) is not TRUE
try(head_tail(x, n = 4.1))
#> Error in checkinput::make_natural(n, strict = TRUE) : 
#>   checkinput::all_natural(n) is not TRUE

df <- head_tail(matrix(data = 1:40, ncol = 4,
                       dimnames = list(LETTERS[1:10], letters[1:4])))
head_tail(df)
#>    a  b  c  d
#> A  1 11 21 31
#> B  2 12 22 32
#> C  3 13 23 33
#> H  8 18 28 38
#> I  9 19 29 39
#> J 10 20 30 40
```
