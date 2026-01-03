#### Create objects to use in tests ####
a <- 1:3
names(a) <- letters[a]
b <- a / 7
c <- 1:10
names(c) <- letters[c]
d <- 5:15
names(d) <- letters[d]

x_in <- c(am = -1, bm = -2, i = 9, j = 10, z = 26, a = 1, b = 2, c = 3)
x_in_InfNa <- c(x_in[1:3], l = NA, m = NaN, n = Inf, o = -Inf, x_in[4:8])
x_in_char <- c(a = "abc", b = "def", c = "this is text")
x_out_named <- paste(names(x_in), x_in, sep = ": ", collapse = ", ")
x_out_nonint <- paste(signif(x = x_in / 8, digits = 2), collapse = ", ")
x_out_char <- "a: abc, b: def, c: this is text"


#### Test the examples ####
expect_identical(numvect_to_char(x = a), "a: 1, b: 2, c: 3")
expect_identical(numvect_to_char(x = a, collapse = NULL), c("a: 1", "b: 2", "c: 3"))
expect_identical(numvect_to_char(x = unname(a)), "1, 2, 3")

expect_identical(
  numvect_to_char(x = b, signif = 7), "a: 0.1428571, b: 0.2857143, c: 0.4285714")
expect_identical(
  numvect_to_char(x = b, signif = 2, sep = " = ", collapse = " and ", width = 15),
  "a = 0.14 and b\n= 0.29 and c =\n0.43")

expect_identical(numvect_to_char(x = x_in_char), x_out_char)
expect_identical(numvect_to_char(x = unname(x_in_char)), "abc, def, this is text")

expect_identical(
  paste0(numvect_to_char(c(table(c(c, d))), sep = " (", collapse =  "), "), ")"),
  paste0("1 (1), 2 (1), 3 (1), 4 (1), 5 (2), 6 (2), 7 (2), 8 (2), 9 (2),",
         " 10 (2), 11 (1), 12 (1), 13 (1), 14 (1), 15 (1)"))


#### Tests ####
expect_identical(numvect_to_char(x = x_in), x_out_named)
expect_identical(numvect_to_char(x = unname(x_in)), paste(x_in, collapse = ", "))

expect_identical(numvect_to_char(x = x_in_char), x_out_char)
expect_identical(numvect_to_char(x = unname(x_in_char)), "abc, def, this is text")

expect_identical(
  numvect_to_char(x = x_in / 8, signif = 2L),
  paste(names(x_in), signif(x = x_in / 8, digits = 2L), sep = ": ", collapse = ", "))
expect_identical(numvect_to_char(x = unname(x_in) / 8, signif = 2L), x_out_nonint)

expect_identical(
  numvect_to_char(x = x_in / 8, signif = 2L),
  paste(names(x_in), signif(x = x_in / 8, digits = 2), sep = ": ", collapse = ", "))
expect_identical(numvect_to_char(x = unname(x_in) / 8, signif = 2L), x_out_nonint)

expect_identical(
  numvect_to_char(x = x_in, sep = "=", collapse = " & "),
  gsub(pattern = ", ", replacement = " & ",
       x = gsub(pattern = ": ", replacement = "=", x = x_out_named, fixed = TRUE),
       fixed = TRUE))

expect_identical(
  numvect_to_char(x = x_in_char, sep = "=", collapse = " & "),
  gsub(pattern = ", ", replacement = " & ",
       x = gsub(pattern = ": ", replacement = "=", x = x_out_char, fixed = TRUE),
       fixed = TRUE))

expect_identical(numvect_to_char(x = x_in_InfNa),
                 paste(names(x_in_InfNa), x_in_InfNa, sep = ": ", collapse = ", "))

# If the use of 'signif_custom()' is implemented, the next test should change to:
# expect_identical(numvect_to_char(x = c(10^c(-1, 5)/7)), "0.0143, 14286")
expect_identical(numvect_to_char(x = c(10^c(-1, 5)/7)), "0.0143, 14300")

expect_warning(expect_identical(
  numvect_to_char(x = list(x_in_InfNa, b, unname(b)), signif = 2),
  paste0("am: -1, bm: -2, i: 9, l: NA, m: NaN, n: Inf, o: -Inf, j: 10, z: 26,",
         " a: 1, b: 2, c: 3, a: 0.14, b: 0.29, c: 0.43, : 0.14, : 0.29, : 0.43")),
  pattern = "Unlisted 'x' to obtain a vector!", strict = TRUE)

expect_error(numvect_to_char(x = data.frame(a = 1:3)),
             pattern = "is.vector(x) is not TRUE", fixed = TRUE)

expect_error(numvect_to_char(x = x_in, signif = 0L),
             pattern = "checkinput::is_positive(signif) is not TRUE", fixed = TRUE)

expect_error(numvect_to_char(x = x_in, signif = c(3L, 4L)),
             pattern = "checkinput::is_positive(signif) is not TRUE", fixed = TRUE)

expect_error(numvect_to_char(x = x_in, sep = c(":", "=")),
             pattern = "checkinput::is_character(sep) is not TRUE", fixed = TRUE)

expect_error(numvect_to_char(x = x_in, sep = NULL),
             pattern = "checkinput::is_character(sep) is not TRUE", fixed = TRUE)

expect_error(numvect_to_char(x = x_in, sep = 3),
             pattern = "checkinput::is_character(sep) is not TRUE", fixed = TRUE)

expect_error(numvect_to_char(x = x_in, collapse = c(":", "=")),
             pattern = "checkinput::is_character(collapse) is not TRUE", fixed = TRUE)

expect_error(numvect_to_char(x = x_in, collapse = 3),
             pattern = "checkinput::is_character(collapse) is not TRUE", fixed = TRUE)

expect_silent(expect_identical(
  numvect_to_char(x = x_in / 8, signif = 2, collapse = NULL),
  paste(names(x_in), signif(x = x_in / 8, digits = 2), sep = ": ", collapse = NULL)))

expect_silent(expect_identical(
  numvect_to_char(x = unname(x_in / 8), signif = 2L, collapse = NULL),
  paste(signif(x = x_in / 8, digits = 2), collapse = NULL)))


#### Remove objects used in tests ####
rm(a, b, c, d, x_in, x_in_char, x_in_InfNa, x_out_char, x_out_named, x_out_nonint)
