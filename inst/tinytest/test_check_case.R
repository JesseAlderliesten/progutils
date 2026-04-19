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

msg_text <- "'x' contains values that only differ in their case: "
msg_text_x <- paste0(msg_text, paste_quoted(x_out))


#### Tests ####
##### Values without problems #####
expect_silent(expect_identical(check_case(x = "a"), character(0)))
expect_silent(expect_identical(check_case(x = "+"), character(0)))
expect_silent(expect_identical(check_case(x = c("a", "b")), character(0)))
expect_silent(expect_identical(check_case(x = c("aB", "aBa")), character(0)))
expect_silent(expect_identical(check_case(x = c("aB", "aba")), character(0)))
expect_silent(expect_identical(check_case(x = fine), character(0)))

##### Values with a single problem #####
# expect_warning(
#   expect_identical(check_case(x = lower_upper_mixed, signal = "warning"),
#                    c("ff", "Ff", "FF", "gg", "gG", "Gg", "GG")),
#   pattern = paste0(msg_text, "'ff', 'Ff', 'FF', 'gg', 'gG', 'Gg', 'GG'"),
#   strict = TRUE, fixed = TRUE
# )
#
# expect_warning(
#   expect_identical(check_case(x = lower_upper, signal = "warning"),
#                    c("h", "H", "j", "J")),
#   pattern = paste0(msg_text, "'h', 'H', 'j', 'J'"), strict = TRUE, fixed = TRUE
# )
#
# expect_warning(
#   expect_identical(check_case(x = lower_mixed, signal = "warning"), c("kk", "Kk")),
#   pattern = paste0(msg_text, "'kk', 'Kk'"), strict = TRUE, fixed = TRUE
# )
#
# expect_warning(
#   expect_identical(check_case(x = upper_mixed, signal = "warning"), c("Ll", "LL")),
#   pattern = paste0(msg_text, "'Ll', 'LL'"), strict = TRUE, fixed = TRUE
# )
#
# expect_warning(
#   expect_identical(check_case(x = mixed_diff, signal = "warning"), c("mM", "Mm")),
#   pattern = paste0(msg_text, "'mM', 'Mm'"), strict = TRUE, fixed = TRUE
# )
#
# ##### Values with multiple problems #####
# expect_error(
#   check_case(x = x, signal = "error"), pattern = msg_text_x, fixed = TRUE
# )
#
# expect_warning(
#   expect_identical(check_case(x = x, signal = "warning"), x_out),
#   pattern = msg_text_x, strict = TRUE, fixed = TRUE
# )
#
# expect_message(
#   expect_identical(check_case(x = x, signal = "message"), x_out),
#   pattern = msg_text_x, strict = TRUE, fixed = TRUE
# )
#
# expect_silent(
#   expect_identical(check_case(x = x, signal = "quiet"), x_out)
# )


#### Remove objects used in tests ####
rm(fine, lower_mixed, lower_upper, lower_upper_mixed, mixed_diff, msg_text,
   msg_text_x, upper_mixed, x, x_out)
