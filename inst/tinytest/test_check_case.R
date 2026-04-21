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
x_out <- c("ff", "Ff", "FF", "gg", "gG", "Gg", "GG", "h", "H", "j", "J",
           "kk", "Kk", "Ll", "LL", "mM", "Mm")
x_out_alt <- c("FF", "Ff", "GG", "Gg", "H", "J", "Kk", "LL", "Ll", "Mm",
               "ff", "gG", "gg", "h", "j", "kk", "mM")

msg_text <- "'x' contains values that only differ in their case: "


#### Tests ####
##### Values without problems #####
expect_silent(expect_identical(check_case(x = "a"), character(0)))
expect_silent(expect_identical(check_case(x = "+"), character(0)))
expect_silent(expect_identical(check_case(x = c("a", "b")), character(0)))
expect_silent(expect_identical(check_case(x = c("aB", "aBa")), character(0)))
expect_silent(expect_identical(check_case(x = c("aB", "aba")), character(0)))
expect_silent(expect_identical(check_case(x = fine), character(0)))

##### Values with a single problem #####
expect_true(
  identical(suppressWarnings(check_case(x = lower_upper_mixed, signal = "warning")),
            c("ff", "Ff", "FF", "gg", "gG", "Gg", "GG")) ||
    identical(suppressWarnings(check_case(x = lower_upper_mixed, signal = "warning")),
              c("FF", "Ff", "GG", "Gg", "ff", "gG", "gg"))
)

expect_warning(
  check_case(x = lower_upper_mixed, signal = "warning"),
  pattern = msg_text, strict = TRUE, fixed = TRUE
)

expect_true(
  identical(suppressWarnings(check_case(x = lower_upper, signal = "warning")),
            c("h", "H", "j", "J")) ||
    identical(suppressWarnings(check_case(x = lower_upper, signal = "warning")),
              c("H", "J", "h", "j"))
)

expect_warning(
  check_case(x = lower_upper, signal = "warning"),
  pattern = msg_text, strict = TRUE, fixed = TRUE
)

expect_true(
  identical(suppressWarnings(check_case(x = lower_mixed, signal = "warning")),
            c("kk", "Kk")) ||
    identical(suppressWarnings(check_case(x = lower_mixed, signal = "warning")),
              c("Kk", "kk"))
)

expect_warning(
  check_case(x = lower_mixed, signal = "warning"),
  pattern = msg_text, strict = TRUE, fixed = TRUE
)

expect_true(
  identical(suppressWarnings(check_case(x = upper_mixed, signal = "warning")), c("Ll", "LL")) ||
    identical(suppressWarnings(check_case(x = upper_mixed, signal = "warning")), c("LL", "Ll"))
)

expect_warning(
  check_case(x = upper_mixed, signal = "warning"),
  pattern = msg_text, strict = TRUE, fixed = TRUE
)

expect_true(
  identical(suppressWarnings(check_case(x = mixed_diff, signal = "warning")),
            c("mM", "Mm")) ||
    identical(suppressWarnings(check_case(x = mixed_diff, signal = "warning")),
              c("Mm", "mM"))
)

expect_warning(
  check_case(x = mixed_diff, signal = "warning"),
  pattern = msg_text, strict = TRUE, fixed = TRUE
)

##### Values with multiple problems #####
expect_error(
  check_case(x = x, signal = "error"),
  pattern = msg_text, fixed = TRUE
)

expect_true(
  identical(suppressWarnings(check_case(x = x, signal = "warning")), x_out) ||
    identical(suppressWarnings(check_case(x = x, signal = "warning")), x_out_alt)
)

expect_warning(
  check_case(x = x, signal = "warning"),
  pattern = msg_text, strict = TRUE, fixed = TRUE
)

expect_true(
  identical(suppressMessages(check_case(x = x, signal = "message")), x_out) ||
    identical(suppressMessages(check_case(x = x, signal = "message")), x_out_alt)
)

expect_message(
  check_case(x = x, signal = "message"),
  pattern = msg_text, strict = TRUE, fixed = TRUE
)

expect_silent(
  expect_true(
    identical(check_case(x = x, signal = "quiet"), x_out) ||
      identical(check_case(x = x, signal = "quiet"), x_out_alt)
  )
)


#### Remove objects used in tests ####
rm(fine, lower_mixed, lower_upper, lower_upper_mixed, mixed_diff, msg_text,
   upper_mixed, x, x_out, x_out_alt)
