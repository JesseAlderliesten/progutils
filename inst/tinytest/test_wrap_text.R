#### Create objects to use in tests ####
x <- "123 567 911 141 719 123 262 931"
nchar_x <- nchar(x)
x_text <- "A piece of text to wrap over multiple lines"
x_text_nospace <- gsub(pattern = " ", replacement = "", x = x_text, fixed = TRUE)
x_last <- "123 567 911 141 719 123 262\n931"
x_multiple <- "123 567 911\n141 719 123\n262 931"
x_multiple_early <- "123\n567\n911 141 719 123 262 931"
x_nonspace_blanks <- "123\f567\n911 141\f719\t123\t262\t931"
x_single <- "123 567\n911 141 719 123 262 931"
expect_x_early <- "123\n567\n911 141 719\n123 262 931"
blank_x_single <- paste0(" \t ", x_single)

warn_too_long <- paste0("Width of 1 text fragments \\(11 characters) exceeds",
                        " 'width' \\(10 characters)")


#### Test the examples ####
expect_identical(wrap_text(x_text, width = 15),
                 "A piece of text\nto wrap over\nmultiple lines")


#### Test other sections ####
##### Value #####
expect_identical(
  wrap_text(x = "a  b \tc\t d\t\te  f   g  h.  i?  j!  k'  l\"  m"),
  "a b c d e f g h.  i?  j!  k' l\" m")


##### Programming note #####
expect_identical(
  wrap_text(x_text, width = 15),
  paste0(strwrap(x_text, width = 15 + 1), collapse = "\n"))
expect_identical(
  wrap_text(x_text, width = 15),
  paste0(strwrap(x_text, width = 15 + 1, prefix = "\n", initial = ""),
         collapse = ""))

##### Note #####
expect_warning(
  expect_identical(
    wrap_text(x = paste0(x_text, x_text_nospace), width = 15),
    paste0("A piece of text\nto wrap over\nmultiple\nlines", x_text_nospace)),
  pattern = paste0("Width of 1 text fragments \\(40 characters) exceeds",
                   " 'width' \\(15 characters)"),
  strict = TRUE)


#### Tests ####
expect_identical(wrap_text(x = "", width = 1L), "")

for(width in (nchar_x - 10L):(nchar_x + 3L)) {
  expect_true(max(lapply(
    X = strsplit(x = wrap_text(x, width = width),
                 split = "\\n", fixed = FALSE),
    FUN = nchar)[[1]]) <= width)
}

# newline ignored, width => nchar_x: x not wrapped, existing newline removed
expect_identical(wrap_text(x = x, width = nchar_x), x)
expect_identical(wrap_text(x = x, width = nchar_x + 1L), x)
expect_identical(wrap_text(x = x_single, width = nchar_x), x)
expect_identical(wrap_text(x = x_single, width = nchar_x + 1L), x)


# newline kept: x wrapped at existing newline
expect_identical(
  wrap_text(x = x_single, width = nchar_x - 8L, ignore_newlines = FALSE),
  x_single)
expect_identical(
  wrap_text(x = x_single, width = nchar_x, ignore_newlines = FALSE),
  x_single)
expect_identical(
  wrap_text(x = x_single, width = nchar_x + 1L, ignore_newlines = FALSE),
  x_single)
expect_identical(
  wrap_text(x = x_single,
            width = nchar_x - regexpr(pattern = "\n", text = x_single)[[1]],
            ignore_newlines = FALSE),
  x_single)

# width < nchar_x: x wrapped at the last space
expect_identical(wrap_text(x = x, width = nchar_x - 1L), x_last)
expect_identical(wrap_text(x = x_single, width = nchar_x - 1L), x_last)

# width < nchar_x but x wrapped at newline that is kept
expect_identical(
  wrap_text(x = x_single, width = nchar_x - 8L, ignore_newlines = FALSE),
  x_single)

##### 'x' ends in a newline #####
# A newline at the end of 'x' is removed even if 'ignore_newlines' is FALSE
expect_identical(
  wrap_text(x = paste0(x, "\n"), width = nchar_x + 99L, ignore_newlines = FALSE),
  x)

##### x wrapped multiple times #####
expect_identical(wrap_text(x = x, width = 12L), x_multiple)
expect_identical(wrap_text(x = x_multiple_early, width = 12L), x_multiple)
expect_identical(wrap_text(x = x_multiple_early, width = 12L,
                           ignore_newlines = FALSE),
                 expect_x_early)

##### Start with blank character #####
# wrap_text() removes leading whitespace
expect_identical(wrap_text(x = paste0(" \t ", x), width = nchar_x + 1L), x)

expect_identical(wrap_text(x = blank_x_single, width = nchar_x + 1L), x)

expect_identical(wrap_text(x = blank_x_single, width = nchar_x + 1L,
                           ignore_newlines = FALSE), x_single)
expect_identical(wrap_text(x = blank_x_single, width = nchar_x - 8L,
                           ignore_newlines = FALSE), x_single)

##### Fragments longer than width #####
expect_warning(expect_identical(
  wrap_text(x = "12345678911", width = 10L),
  "12345678911"),
  pattern = warn_too_long, strict = TRUE)

expect_warning(expect_identical(
  wrap_text(x = "12345678911 XXX YYY ZZZZZZ", width = 10L),
  "12345678911\nXXX YYY\nZZZZZZ"),
  pattern = warn_too_long, strict = TRUE)

expect_warning(expect_identical(
  wrap_text(x = "XXX YYY 12345678911 ZZZZZZ", width = 10L),
  "XXX YYY\n12345678911\nZZZZZZ"),
  pattern = warn_too_long, strict = TRUE)

expect_warning(expect_identical(
  wrap_text(x = "XXX YYY ZZZZZZ 12345678911", width = 10L),
  "XXX YYY\nZZZZZZ\n12345678911"),
  pattern = warn_too_long, strict = TRUE)

expect_warning(expect_identical(
  wrap_text(x = "123456789111315 XXX YYY 1234567891113 ZZZZZZ 12345678911",
            width = 10L),
  "123456789111315\nXXX YYY\n1234567891113\nZZZZZZ\n12345678911"),
  pattern = paste0("Width of 3 text fragments \\(15, 13, 11 characters)",
                   " exceeds 'width' \\(10 characters)"), strict = TRUE)

##### x of length > 1 #####
expect_identical(
  wrap_text(x = c(x, x_single), width = 11L),
  paste0(x_multiple, " 123\n567 911 141\n719 123 262\n931"))

expect_identical(
  wrap_text(x = c(x, x_single), width = 11L, ignore_newlines = FALSE),
  paste0(c(x_multiple, expect_x_early), collapse = " "))

expect_identical(
  wrap_text(x = c(x, x_single), width = 100L),
  paste0(c(x, x), collapse = " "))

##### x with non-space blanks #####
expect_identical(
  wrap_text(x = x_nonspace_blanks, width = 11L, ignore_newlines = FALSE),
  "123\f567\n911 141\f719\n123 262 931")

##### Error for width < 6L #####
expect_error(
  wrap_text(x = x, width = 0L),
  pattern = "is_positive\\(width) is not TRUE")

expect_warning(expect_identical(
  wrap_text(x = x, width = 1L), "123\n567\n911\n141\n719\n123\n262\n931"),
  pattern = paste0("Width of 8 text fragments \\(3, 3, 3, 3, 3, 3, 3, 3",
                   " characters) exceeds 'width' \\(1 characters)"), strict = TRUE)


#### Remove objects used in tests ####
rm(expect_x_early, width, blank_x_single, nchar_x,
   warn_too_long, x, x_last, x_multiple, x_multiple_early,
   x_nonspace_blanks, x_single, x_text, x_text_nospace)
