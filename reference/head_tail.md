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

  [make_natural](https://jessealderliesten.github.io/checkinput/reference/is_natural.html)
  number (default `3L`) indicating the number of elements or rows to
  return as `head` and `tail`.

## Value

The `n` first and `n` last elements or rows of `x`. `x` is returned
completely if `n` is not less than half the number of elements or rows
of `x`. Using `n = 0` results in a zero-length or zero-row object.

## See also

[`utils::head()`](https://rdrr.io/r/utils/head.html)
[`utils::tail()`](https://rdrr.io/r/utils/head.html)

## Examples

``` r
x <- letters
names(x) <- LETTERS
head_tail(x)
#>   A   B   C   X   Y   Z 
#> "a" "b" "c" "x" "y" "z" 
head_tail(x, n = 4)
#>   A   B   C   D   W   X   Y   Z 
#> "a" "b" "c" "d" "w" "x" "y" "z" 
head_tail(x, n = 40)
#>   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   P   Q   R   S   T 
#> "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" 
#>   U   V   W   X   Y   Z 
#> "u" "v" "w" "x" "y" "z" 
try(head_tail(x, n = 0))
#> Error in checkinput::make_natural(n, strict = TRUE) : 
#>   checkinput::all_natural(n) is not TRUE
try(head_tail(x, n = 4.1))
#> Error in checkinput::make_natural(n, strict = TRUE) : 
#>   checkinput::all_natural(n) is not TRUE

m <- head_tail(matrix(data = 1:40, ncol = 4,
                      dimnames = list(LETTERS[1:10], letters[1:4])))
head_tail(m)
#>    a  b  c  d
#> A  1 11 21 31
#> B  2 12 22 32
#> C  3 13 23 33
#> H  8 18 28 38
#> I  9 19 29 39
#> J 10 20 30 40
head_tail(as.data.frame(m))
#>    a  b  c  d
#> A  1 11 21 31
#> B  2 12 22 32
#> C  3 13 23 33
#> H  8 18 28 38
#> I  9 19 29 39
#> J 10 20 30 40
```
