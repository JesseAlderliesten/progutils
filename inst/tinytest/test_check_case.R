#### Create objects to use in tests ####
x <- c("a", "b", "C", "D")
y <- c("a", "B", "c", "D")
z <- c("a", "b", "C", "D", "A")

msg_contains <- "contains values that only differ in their case"
msg_text <- paste0("'x' ", msg_contains, " from values in 'y':\n'b', 'C'")

#### Test the examples ####
expect_error(check_case(x = x, y = y, signal = "error"), pattern = msg_text)

expect_warning(
  expect_identical(check_case(x = x, y = y, signal = "warning"), c("b", "C")),
  pattern = msg_text, strict = TRUE)

expect_silent(
  expect_identical(check_case(x = x, y = "h", signal = "warning"), character(0))
)

expect_message(
  expect_identical(
    check_case(x = z, y = NULL, signal = "warning", signal_within = "message"),
    c("a", "A")),
  pattern = paste0("'x' ", msg_contains, ": 'a', 'A'"), strict = TRUE
)

expect_warning(
  expect_identical(
    check_case(x = z, y = "h", signal = "warning", signal_within = "warning"),
    character(0)),
  pattern = paste0("'x' ", msg_contains, ": 'a', 'A'"), strict = TRUE
)

expect_silent(
  expect_identical(
    check_case(x = z, y = "h", signal = "warning", signal_within = "quiet"),
    character(0))
)

expect_warning(
  expect_identical(check_case(x = x, y = z, signal = "warning"), "a"),
  pattern = paste0("'y' ", msg_contains, ": 'a', 'A'"), strict = TRUE
)

expect_warning(
  expect_identical(check_case(x = x, y = z, signal = "warning"), "a"),
  pattern = paste0("'x' ", msg_contains, " from values in 'y':\n'a'"),
  strict = TRUE
)

#### Additional tests ####
# 'x' and 'y' might be 'character(0)', '""', or NA_character_
expect_silent(expect_identical(check_case(x = character(0), y = "a"), character(0)))
expect_silent(expect_identical(check_case(x = "a", y = character(0)), character(0)))
expect_silent(expect_identical(check_case(x = "", y = ""), character(0)))
expect_silent(
  expect_identical(check_case(x = NA_character_, y = NA_character_), character(0))
)

#### Erroneous input ####
# 'x' should be a non-NULL character value without dimensions
expect_error(check_case(x = NULL, y = "a"),
             pattern = "is.character(x) is not TRUE", fixed = TRUE)
expect_error(check_case(x = 1, y = "a"),
             pattern = "is.character(x) is not TRUE", fixed = TRUE)
expect_error(check_case(x = matrix("a"), y = "a"),
             pattern = "is.null(dim(x)) is not TRUE", fixed = TRUE)

# 'y' should be a character value without dimensions if it is not NULL
expect_silent(
  expect_identical(check_case(x = "a", y = NULL), character(0))
)
expect_error(check_case(x = "a", y = 1),
             pattern = "is.character(y) is not TRUE", fixed = TRUE)
expect_error(check_case(x = "a", y = matrix("a")),
             pattern = "is.null(dim(y)) is not TRUE", fixed = TRUE)

#### Remove objects used in tests ####
rm(msg_contains, msg_text, x, y, z)
