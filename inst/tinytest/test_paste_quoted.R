#### Notes ####
# - This file tests the exported function progutils::paste_quoted().
# - A leaner, internal version of this function is checkinput:::paste_quoted().
# - Also adjust the tests of checkinput:::paste_quoted() when making changes here!


#### Create objects to use in tests ####
list_input <-  list("a", c("a", "b"), c("a", "b", "a"),
                    c(3, 4), NULL, NA, NA_character_)
list_output <- list("'a'", "'a', 'b'", "'a', 'b', 'a'",
                    "'3', '4'", "'NULL'", "'NA'", "'NA'")
list_input_zerolength <- list(NULL, character(0), numeric(0), logical(0),
                              vector(mode = "list"), "")
list_output_zerolength <- list("'NULL'", "'character(0)'", "'numeric(0)'",
                               "'logical(0)'", "'list(0)'", "''")
x_fact_ind <- c(4:6, 5L)
x_fact <- as.factor(letters[x_fact_ind])
x_fact_int <- as.factor(x_fact_ind)
x_fact_num <- as.factor(x_fact_ind / 16)


#### Test the examples ####
expect_identical(paste_quoted(c(3, 4)), "'3', '4'")
expect_warning(expect_identical(
  paste_quoted(c(a = 3, b = 4)), "'3', '4'"),
  pattern = "'x' has names, these will be discarded. Use vect_to_char",
  strict = TRUE, fixed = TRUE)
expect_identical(paste_quoted(NULL), "'NULL'") # Also mentioned in the 'Note'


#### Test other sections ####
# 'Note'
expect_identical(paste_quoted(logical(0)), "'logical(0)'")
expect_silent(expect_identical(paste_quoted(c("a", "b")), "'a', 'b'"))
expect_identical(toString(c("a", "b")), paste(c("a", "b"), collapse = ", "))


#### Tests ####
for(index in seq_along(list_input)) {
  expect_identical(paste_quoted(x = list_input[[index]]), list_output[[index]])
}

expect_silent(expect_identical(paste_quoted(x = x_fact), "'d', 'e', 'f', 'e'"))
expect_silent(expect_identical(paste_quoted(x = x_fact_int), "'4', '5', '6', '5'"))
expect_silent(expect_identical(paste_quoted(x = x_fact_num),
                               "'0.25', '0.3125', '0.375', '0.3125'"))

expect_error(
  paste_quoted(3, 4),
  pattern = "unused argument (4)", fixed = TRUE)

expect_error(
  paste_quoted(c(3, 4), 5:6),
  pattern = "unused argument (5:6)", fixed = TRUE)

expect_error(
  paste_quoted(c(3, 4), 5:6, 7),
  pattern = "unused arguments (5:6, 7)", fixed = TRUE)

expect_error(
  paste_quoted(c(3, 4), h = 5, 7),
  pattern = "unused arguments (h = 5, 7)", fixed = TRUE)

for(index_NULL in seq_along(list_input_zerolength)) {
  expect_silent(expect_identical(
    paste_quoted(list_input_zerolength[[index_NULL]]),
    list_output_zerolength[[index_NULL]]))
}

for(x in list(data.frame(a = 314), as.matrix(data.frame(a = 314)))) {
  expect_error(
    paste_quoted(x),
    pattern = "is.vector(x) || is.factor(x) || is.null(x) is not TRUE", fixed = TRUE)
}


#### Remove objects used in tests ####
rm(index, index_NULL, list_input, list_input_zerolength, list_output,
   list_output_zerolength, x, x_fact, x_fact_ind, x_fact_int, x_fact_num)
