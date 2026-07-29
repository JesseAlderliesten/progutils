#### Create objects to use in tests ####
x <- "'ff', 'gG', 'HH'"
y <- "'ff'; 'gG', 'HH'"
sep_quotes <- c("'ff'", "'gG'", "'HH'")


#### Tests ####
expect_silent(
  expect_identical(
    paste_quoted(x = c("ff", "gG", "HH")),
    x)
)

expect_silent(
  expect_identical(
    paste_quoted(x = unpaste_unquote(x)),
    x)
)

expect_silent(
  expect_identical(
    unpaste_unquote(x = x, collapse = ", ", quotemarks = "'"),
    c("ff", "gG", "HH"))
)

expect_silent(
  expect_identical(
    unpaste_unquote(x = y, collapse = "; ", quotemarks = "'"),
    c("ff", "gG, HH"))
)

expect_silent(
  expect_identical(
    unpaste_unquote(x = y, collapse = c(", ", "; "), quotemarks = "'"),
    c("ff", "gG", "HH"))
)

expect_silent(
  expect_identical(
    unpaste_unquote(x = x, collapse = ", ", quotemarks = "\""),
    sep_quotes)
)

expect_silent(
  expect_identical(
    unpaste_unquote(x = y, collapse = "; ", quotemarks = "\""),
    c("'ff'", "'gG', 'HH'"))
)

expect_silent(
  expect_identical(
    unpaste_unquote(x = y, collapse = c(", ", "; "), quotemarks = "\""),
    sep_quotes)
)

expect_silent(
  expect_identical(
    unpaste_unquote(x = x, collapse = character(0), quotemarks = c("'", "\"")),
    "ff, gG, HH")
)

expect_silent(
  expect_identical(
    unpaste_unquote(x = y, collapse = character(0), quotemarks = c("'", "\"")),
    "ff; gG, HH")
)

expect_silent(
  expect_identical(
    unpaste_unquote(x = y, collapse = character(0), quotemarks = "\""),
    y)
)

expect_silent(
  expect_identical(
    unpaste_unquote(x = x, collapse = ", ", quotemarks = character(0)),
    sep_quotes)
)

expect_silent(
  expect_identical(
    unpaste_unquote(x = y, collapse = "; ", quotemarks = character(0)),
    c("'ff'", "'gG', 'HH'"))
)

expect_silent(
  expect_identical(
    unpaste_unquote(x = y, collapse = c(", ", "; "), quotemarks = character(0)),
    sep_quotes)
)

expect_silent(
  expect_identical(
    unpaste_unquote(x = x, collapse = character(0), quotemarks = character(0)),
    x)
)

expect_silent(
  expect_identical(
    unpaste_unquote(x = y, collapse = character(0), quotemarks = character(0)),
    y)
)

# Characters denoting NAs
expect_silent(
  expect_identical(
    unpaste_unquote(x = "'NA_character_'", collapse = ", ", quotemarks = "'"),
    "NA_character_")
)

expect_silent(
  expect_identical(
    unpaste_unquote(x = "'NA_real_'", collapse = ", ", quotemarks = "'"),
    "NA_real_")
)

# Characters denoting zero-length input
expect_silent(
  expect_identical(
    unpaste_unquote(x = "'a', 'NULL', 'character(0)', 'numeric(0)', 'b'",
                    collapse = ", ", quotemarks = "'"),
    c("a", "NULL", "character(0)", "numeric(0)", "b"))
)

expect_silent(
  expect_identical(
    unpaste_unquote(x = "'NULL', 'character(0)', 'numeric(0)'",
                    collapse = ", ", quotemarks = "'"),
    c("NULL", "character(0)", "numeric(0)"))
)

expect_silent(
  expect_identical(
    unpaste_unquote(x = "'NULL'", collapse = ", ", quotemarks = "'"),
    "NULL")
)

expect_silent(
  expect_identical(
    unpaste_unquote(x = "'character(0)'", collapse = ", ", quotemarks = "'"),
    "character(0)")
)

expect_silent(
  expect_identical(
    unpaste_unquote(x = "'numeric(0)'", collapse = ", ", quotemarks = "'"),
    "numeric(0)")
)

##### Erroneous input #####
expect_error(
  unpaste_unquote(x = x, collapse = "", quotemarks = c("'", "\"")),
  pattern = "checkinput::all_characters(collapse, allow_zerolength = TRUE)",
  fixed = TRUE
)

expect_error(
  unpaste_unquote(x = x, collapse = NULL, quotemarks = c("'", "\"")),
  pattern = "checkinput::all_characters(collapse, allow_zerolength = TRUE)",
  fixed = TRUE
)

expect_error(
  unpaste_unquote(x = x, collapse = ", ", quotemarks = ""),
  pattern = "checkinput::all_characters(quotemarks, allow_zerolength = TRUE)",
  fixed = TRUE
)

expect_error(
  unpaste_unquote(x = x, collapse = ", ", quotemarks = NULL),
  pattern = "checkinput::all_characters(quotemarks, allow_zerolength = TRUE)",
  fixed = TRUE
)


#### Remove objects used in tests ####
rm(sep_quotes, x, y)
