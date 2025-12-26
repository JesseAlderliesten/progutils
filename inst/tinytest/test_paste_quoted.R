#### Create objects to use in tests ####
list_input <-  list("a", c("a", "b"), c("a", "b", "a"),
                    c(3, 4), NULL, NA, NA_character_)
list_output <- list("'a'", "'a', 'b'", "'a', 'b', 'a'",
                    "'3', '4'", "'NULL'", "'NA'", "'NA'")
list_input_zerolength <- list(NULL, character(0), numeric(0), logical(0), "")
list_output_zerolength <- list("'NULL'", "'NULL'", "''", "''", "''")

warn_ignored <- paste0("unnamed arguments that are not formal arguments",
                       " of paste_quoted\\(). Did\nyou forget to combine 'x'")


#### Test the examples ####
expect_identical(paste_quoted(c(3, 4)), "'3', '4'")
expect_warning(expect_identical(
  paste_quoted(c(a = 3, b = 4)), "'3', '4'"),
  pattern = "'x' has names, these will be discarded. Use numvect_to_char",
  strict = TRUE)
expect_identical(paste_quoted(NULL), "'NULL'")


#### Test other sections ####
# 'Details'
expect_warning(expect_identical(paste_quoted("a", "b"), "'a'"),
               pattern = paste0("Ignored 1 ", warn_ignored), strict = TRUE)

# 'Note'
expect_identical(paste_quoted(NULL), "'NULL'")
expect_identical(paste_quoted(character(0)), "'NULL'")
expect_identical(paste_quoted(logical(0)), "''")
expect_identical(toString(c("a", "b")), paste(c("a", "b"), collapse = ", "))


#### Tests ####
for(index in seq_along(list_input)) {
  expect_identical(paste_quoted(x = list_input[[index]]), list_output[[index]])
}

expect_warning(expect_identical(
  paste_quoted(3, 4), "'3'"),
  pattern = paste0("Ignored 1 ", warn_ignored), strict = TRUE)

expect_warning(expect_identical(
  paste_quoted(c(3, 4), 5:6), "'3', '4'"),
  pattern = paste0("Ignored 1 ", warn_ignored), strict = TRUE)

expect_warning(expect_identical(
  paste_quoted(c(3, 4), 5:6, 7), "'3', '4'"),
  pattern = paste0("Ignored 2 ", warn_ignored), strict = TRUE)

expect_warning(expect_identical(
  paste_quoted(c(3, 4), h = 5, 7), "'3', '4'"),
  pattern = paste0("Ignored arguments 'h' and 1 unnamed arguments that are not",
                   " formal arguments of"), strict = TRUE)

expect_warning(expect_identical(
  paste_quoted(c(a = 3, b = 4)), "'3', '4'"),
  pattern = "'x' has names, these will be discarded. Use numvect_to_char",
  strict = TRUE)

for(index_NULL in seq_along(list_input_zerolength)) {
  expect_identical(
    paste_quoted(list_input_zerolength[[index_NULL]]),
    list_output_zerolength[[index_NULL]])
}


#### Remove objects used in tests ####
rm(index, index_NULL, list_input, list_input_zerolength, list_output,
   list_output_zerolength, warn_ignored)
