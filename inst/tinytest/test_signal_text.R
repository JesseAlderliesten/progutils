#### Create objects to use in tests ####
test_text <- c("Some text", "Some other\ntext")
test_text_out <- vect_to_char(test_text, ignore_newlines = FALSE)
test_numbers <- 11:13
test_numbers_out <- vect_to_char(test_numbers)
test_numbers_out_rounded <- "1.57, 1.71, 1.86"
test_numbers_out_vect <- "1.571, 1.714,\n1.857"
test_text_out_origin <- paste0(test_text_out,
                               "\n< This message originated from: origin_text. >")
x_negative <- -1
attributes(x_negative) <- list(text_signal = "This is a negative number")
x_positive <- 1
attributes(x_positive) <- list(text_signal = "")

signal_negative <- function(x, signal = c("error", "warning", "message", "quiet")) {
  text_signal <- "This is a negative number"
  if(x < 0) {
    progutils::signal_text(text = text_signal, signal = signal)
    attributes(x) <- list(text_signal = text_signal)
  } else {
    attributes(x) <- list(text_signal = "")
  }
  x
}


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

expect_message(
  expect_equal(
    signal_text(text = test_numbers, signal = "message"),
    test_numbers_out),
  pattern = test_numbers_out, fixed = TRUE)

expect_warning(
  expect_identical(
    signal_negative(x = -1, signal = "warning"),
    x_negative
  ),
  pattern = "This is a negative number", strict = TRUE, fixed = TRUE
)

expect_silent(
  expect_identical(
    signal_negative(x = 1, signal = "warning"),
    x_positive
  )
)

expect_true(
  grepl(pattern = "negative", x = attr(x_negative, "text_signal"),
        ignore.case = FALSE, fixed = TRUE)
)
expect_false(
  grepl(pattern = "negative", x = attr(x_positive, "text_signal"),
        ignore.case = FALSE, fixed = TRUE)
)

expect_message(
  message(test_numbers),
  pattern = paste0(test_numbers, collapse = ""), fixed = TRUE)

expect_message(
  expect_equal(
    signal_text(text = test_numbers / 7, signal = "message"),
    test_numbers_out_rounded),
  pattern = test_numbers_out_rounded, fixed = TRUE)

expect_message(
  expect_equal(
    signal_text(text = vect_to_char(x = test_numbers / 7, signif = 4, width = 15),
                signal = "message"),
    test_numbers_out_vect),
  pattern = test_numbers_out_vect, fixed = TRUE)

expect_error(
  signal_text(text = test_text, signal = "abc"),
  pattern = "'arg' should be one of ", fixed = TRUE)

expect_error(
  signal_text(text = test_text, signal = "error", origin = NA_character_),
  pattern = paste0("checkinput::is_character(origin, allow_zerolength = TRUE)",
                   " is not TRUE"),
  fixed = TRUE)

expect_error(
  signal_text(text = test_text, signal = "error", origin = 1),
  pattern = paste0("checkinput::is_character(origin, allow_zerolength = TRUE)",
                   " is not TRUE"),
  fixed = TRUE)


#### Remove objects used in tests ####
rm(signal_negative, test_numbers, test_numbers_out, test_numbers_out_rounded,
   test_numbers_out_vect, test_text, test_text_out, test_text_out_origin,
   x_negative, x_positive)
