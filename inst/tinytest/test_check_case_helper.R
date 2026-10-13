#### Create objects to use in tests ####
lower_upper_mixed <- c("ff", "FF", "Ff", "GG", "Gg", "gG", "gg", "FF", "ff")
lower_upper <- c("H", "h", "J", "J", "j", "j")
lower_mixed <- c("Kk", "kk")
upper_mixed <- c("LL", "Ll")
mixed_diff <- c("Mm", "mM")
fine <- c("+", "-", "-", "2", "2", "", NA_character_, "a", "b", "b", "C",
          "D", "D", "Ee", "Ee", "Nn")
x <- c(lower_upper_mixed, lower_upper, lower_mixed, upper_mixed,
       mixed_diff, fine)
x_out <- c("ff", "FF", "Ff", "GG", "Gg", "gG", "gg", "H", "h", "J", "j", "Kk",
           "kk", "LL", "Ll", "Mm", "mM")

msg_text <- "contains values that only differ in their case: "
error_x <- "checkinput::all_characters(x, allow_empty = TRUE,"


#### Tests ####
##### Values without problems #####
expect_silent(expect_identical(progutils:::check_case_helper(x = "a"), character(0)))
expect_silent(expect_identical(progutils:::check_case_helper(x = "+"), character(0)))
expect_silent(expect_identical(progutils:::check_case_helper(x = c("a", "b")), character(0)))
expect_silent(expect_identical(progutils:::check_case_helper(x = c("aB", "aBa")), character(0)))
expect_silent(expect_identical(progutils:::check_case_helper(x = c("aB", "aba")), character(0)))
expect_silent(expect_identical(progutils:::check_case_helper(x = fine), character(0)))

##### Values with a single problem #####
expect_warning(
  expect_identical(
    progutils:::check_case_helper(x = lower_upper_mixed, signal = "warning"),
    unique(lower_upper_mixed)),
  pattern = paste0("'lower_upper_mixed' ", msg_text,
                   paste_quoted(unique(lower_upper_mixed))),
  strict = TRUE, fixed = TRUE
)

expect_warning(
  expect_identical(
    progutils:::check_case_helper(x = rev(lower_upper_mixed), signal = "warning"),
    unique(rev(lower_upper_mixed))),
  pattern = paste0("'rev(lower_upper_mixed)' ", msg_text,
                   paste_quoted(unique(rev(lower_upper_mixed)))),
  strict = TRUE, fixed = TRUE
)

expect_warning(
  expect_identical(
    progutils:::check_case_helper(x = lower_upper, signal = "warning"),
    c("H", "h", "J", "j")),
  pattern = paste0("'lower_upper' ", msg_text,
                   paste_quoted(c("H", "h", "J", "j"))),
  strict = TRUE, fixed = TRUE
)

expect_warning(
  expect_identical(
    progutils:::check_case_helper(x = lower_mixed, signal = "warning"),
    c("Kk", "kk")),
  pattern = paste0("'lower_mixed' ", msg_text, paste_quoted(c("Kk", "kk"))),
  strict = TRUE, fixed = TRUE
)

expect_warning(
  expect_identical(
    progutils:::check_case_helper(x = upper_mixed, signal = "warning"),
    c("LL", "Ll")),
  pattern = paste0("'upper_mixed' ", msg_text, paste_quoted(c("LL", "Ll"))),
  strict = TRUE, fixed = TRUE
)

expect_warning(
  expect_identical(
    progutils:::check_case_helper(x = mixed_diff, signal = "warning"),
    c("Mm", "mM")),
  pattern = paste0("'mixed_diff' ", msg_text, paste_quoted(c("Mm", "mM"))),
  strict = TRUE, fixed = TRUE
)

##### Values with multiple problems #####
expect_error(
  progutils:::check_case_helper(x = x, signal = "error"),
  pattern = paste0("'x' ", msg_text), fixed = TRUE
)

expect_warning(
  expect_identical(
    progutils:::check_case_helper(x = x, signal = "warning"),
    x_out),
  pattern = paste0("'x' ", msg_text, paste_quoted(x_out)),
  strict = TRUE, fixed = TRUE
)

expect_message(
  expect_identical(
    progutils:::check_case_helper(x = x, signal = "message"),
    x_out),
  pattern = paste0("'x' ", msg_text, paste_quoted(x_out)),
  strict = TRUE, fixed = TRUE
)

expect_silent(
  expect_identical(
    progutils:::check_case_helper(x = x, signal = "quiet"),
    x_out)
)

#### Erroneous input ####
# 'x' should be a non-NULL character value without dimensions
expect_error(progutils:::check_case_helper(x = NULL), pattern = error_x, fixed = TRUE)
expect_error(progutils:::check_case_helper(x = 1), pattern = error_x, fixed = TRUE)
expect_error(progutils:::check_case_helper(x = matrix("a")), pattern = error_x, fixed = TRUE)

#### Remove objects used in tests ####
rm(error_x, fine, lower_mixed, lower_upper, lower_upper_mixed, mixed_diff,
   msg_text, upper_mixed, x, x_out)
