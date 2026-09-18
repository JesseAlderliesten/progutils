#### Tests ####

##### Test the examples #####
expect_silent(expect_identical(replace_nonalnum("a+b.txt"), "a_b.txt"))
expect_silent(expect_identical(replace_nonalnum("a b.txt"), "a_b.txt"))
expect_silent(expect_identical(replace_nonalnum("ab12._"), "ab12._")) # no change
expect_silent(expect_identical(
  replace_nonalnum(c("a+b.txt", "ab12._")), c("a_b.txt", "ab12._")
))
expect_silent(expect_identical(
  replace_nonalnum("a+b.txt", replacement = ""), "ab.txt"
))
expect_silent(expect_identical(
  replace_nonalnum("a+bc_d.txt", replacement = "", keep_underscore = TRUE), "abc_d.txt"
))
expect_silent(expect_identical(
  replace_nonalnum("a+bc_d.txt", replacement = "", keep_underscore = FALSE), "abcd.txt"
))
expect_silent(expect_identical(
  replace_nonalnum("a+bc_d.txt", keep_underscore = FALSE), "a_bc_d.txt"
))

##### Other tests #####
expect_silent(expect_identical(
  replace_nonalnum(c("", "a+b.txt", "ab12._")), c("", "a_b.txt", "ab12._")
))
expect_silent(expect_identical(
  replace_nonalnum(c("", "a+b.txt", "ab12._"), replacement = ""),
  c("", "ab.txt", "ab12._")
))
expect_silent(expect_identical(
  replace_nonalnum(c("a+b.txt", "ab12._", "")), c("a_b.txt", "ab12._", "")
))
expect_silent(expect_identical(
  replace_nonalnum(c("", "")), c("", "")
))

##### Values that should result in an error #####
expect_error(
  replace_nonalnum(x = 3),
  pattern = "checkinput::all_characters(x, allow_empty = TRUE) is not TRUE",
  fixed = TRUE
)

expect_error(
  replace_nonalnum(x = "a+bc_d.txt", replacement = 3),
  pattern = "checkinput::is_character(replacement, allow_empty = TRUE) is not TRUE",
  fixed = TRUE
)

expect_error(
  replace_nonalnum(x = "a+bc_d.txt", replacement = c("a", "b")),
  pattern = "checkinput::is_character(replacement, allow_empty = TRUE) is not TRUE",
  fixed = TRUE
)

expect_error(
  replace_nonalnum(x = "a+bc_d.txt", keep_underscore = NA),
  pattern = "checkinput::is_logical(keep_underscore) is not TRUE",
  fixed = TRUE
)

expect_error(
  replace_nonalnum(x = "a+bc_d.txt", keep_underscore = c(TRUE, FALSE)),
  pattern = "checkinput::is_logical(keep_underscore) is not TRUE",
  fixed = TRUE
)
