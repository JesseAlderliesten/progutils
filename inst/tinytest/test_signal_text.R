#### Create objects to use in tests ####
test_text <- c("Some text", "Some other\ntext")
test_text_out <- vect_to_char(test_text, ignore_newlines = FALSE)
test_numbers <- 11:13
test_numbers_out <- vect_to_char(test_numbers)
test_text_out_origin <- paste0(test_text_out,
                               "\n< This message originated from: origin_text. >")


#### Tests ####
expect_error(
  signal_text(text = test_text, signal = "error"),
  pattern = test_text_out, fixed = TRUE)

expect_warning(
  expect_identical(
    signal_text(text = test_text, signal = "warn"),
    test_text_out),
  pattern = test_text_out, fixed = TRUE)

# match.arg uses partial matching
expect_warning(
  expect_identical(
    signal_text(text = test_text, signal = "w"),
    test_text_out),
  pattern = test_text_out, fixed = TRUE)

expect_message(
  expect_identical(
    signal_text(text = test_text, signal = "message"),
    test_text_out),
  pattern = test_text_out, strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    signal_text(text = test_text, signal = "message", origin = "origin_text"),
    test_text_out_origin),
  pattern = test_text_out_origin, strict = TRUE, fixed = TRUE)

expect_silent(
  expect_identical(
    signal_text(text = test_text, signal = "quiet"),
    test_text_out)
)

expect_error(
  signal_text(text = test_numbers, signal = "error"),
  pattern = test_numbers_out, fixed = TRUE)

expect_error(
  signal_text(text = test_text, signal = "abc"),
  pattern = "'arg' should be one of ", fixed = TRUE)

expect_error(
  signal_text(text = test_text, signal = "error", origin = NA_character_),
  pattern = "checkinput::is_character(origin, allow_zero = TRUE) is not TRUE", fixed = TRUE)

expect_error(
  signal_text(text = test_text, signal = "error", origin = 1),
  pattern = "checkinput::is_character(origin, allow_zero = TRUE) is not TRUE", fixed = TRUE)


#### Remove objects used in tests ####
rm(test_numbers, test_numbers_out, test_text, test_text_out, test_text_out_origin)
