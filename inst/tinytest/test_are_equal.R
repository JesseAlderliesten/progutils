#### Create objects to use in tests ####
z <- c(-Inf, -3 - 1e-8, -3, -1e-8, 0, 1e-8, 3 - 1e-8, 3, Inf, NA_real_, NaN)


#### Test the examples ####
expect_silent(expect_false(sqrt(2)^2 == 2))
expect_silent(expect_true(are_equal(x = sqrt(2)^2, y = 2)))

expect_silent(
  expect_identical(
    are_equal(x = c(2, 3,        3, NA, Inf),
              y = c(2, 3, 3 + 1e-8, NA, Inf)),
    c(rep(TRUE, 3L), rep(NA, 2L))))

expect_silent(
  expect_identical(
    are_equal(x = 3, y = c(2, 3, 3 + 1e-8, NA, Inf)),
    c(FALSE, TRUE, TRUE, NA, FALSE)))


#### Tests ####
out_temp <- c(NA, NA, rep(FALSE, 3L), TRUE, TRUE)
out <- c(NA, rep(FALSE, 8L),
         rep(c(NA, NA, FALSE, TRUE, TRUE, rep(FALSE, 6L)), 2L),
         out_temp, rep(FALSE, 4L), out_temp, TRUE, rep(FALSE, 3L), NA, NA,
         rep(FALSE, 4L), TRUE, TRUE, rep(FALSE, 3L),
         rep(c(NA, NA, rep(FALSE, 6L), TRUE, TRUE, FALSE), 2L), NA, NA,
         rep(FALSE, 8L), rep(NA, 25L))
expect_silent(expect_identical(
  are_equal(x = rep(z, length(z)), y = rep(z, each = length(z))), out))

# Recycle scalar input
expect_silent(expect_identical(
  are_equal(x = 1:4, y = 3 + 1e-8),
  c(FALSE, FALSE, TRUE, FALSE)))

# Error on non-compatible vector lengths
expect_error(are_equal(x = rep(3, 4L), y = c(3, 4)),
             pattern = "Lengths of 'x' (4) and 'y' (2) are not compatible",
             fixed = TRUE)

# Error on non-numeric arguments
expect_error(are_equal(x = "a", y = c(3, 4)),
             pattern = "checkinput::all_numbers(x", fixed = TRUE)
expect_error(are_equal(x = c(3, 4), y = "a"),
             pattern = "checkinput::all_numbers(y", fixed = TRUE)
expect_error(are_equal(x = NA, y = 4),
             pattern = "checkinput::all_numbers(x", fixed = TRUE)
expect_error(are_equal(x = c(3, 4), y = c(3, 4), tol = "a"),
             pattern = "checkinput::is_positive(tol) is not TRUE",
             fixed = TRUE)

# Error on negative or zero input to 'tol'
expect_error(are_equal(x = c(3, 4), y = c(3, 4), tol = -1),
             pattern = "checkinput::is_positive(tol) is not TRUE", fixed = TRUE)
expect_error(are_equal(x = c(3, 4), y = c(3, 4), tol = 0),
             pattern = "checkinput::is_positive(tol) is not TRUE", fixed = TRUE)


#### Remove objects used in tests ####
rm(out, out_temp, z)
