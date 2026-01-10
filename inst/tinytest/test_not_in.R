#### Create objects to use in tests ####
x <- c("shared_both_double", "shared_x_double_table_single",
       "only_x_double", "only_x_single",
       "shared_both_single", "shared_x_single_table_double",
       "only_x_double", "shared_x_double_table_single",
       "shared_both_double")
table <- c("only_table_double", "shared_both_double",
           "shared_x_single_table_double", "shared_both_single",
           "shared_x_single_table_double",
           "shared_x_double_table_single", "shared_both_double",
           "only_table_single", "only_table_double")
out_boolean <- c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)


#### Test the examples ####
x_example <- letters[1:4]
table_example <- letters[3:6]
x_example_dupl <- c(x_example, letters[c(2, 4:6, 5)])
table_example_dupl <- letters[c(3:8, 5:7)]

expect_silent(expect_identical(not_in(x_example, table_example), c("a", "b")))
expect_silent(expect_identical(
  not_in(as.factor(x_example), as.factor(table_example)), c("a", "b")))
expect_silent(expect_identical(
  not_in(x_example, table_example, value = FALSE),
  c(TRUE, TRUE, FALSE, FALSE)))
expect_silent(expect_identical(
  not_in(x_example, table_example, value = FALSE),
  !(x_example %in% table_example)))

expect_silent(expect_identical(
  not_in(x_example_dupl, table_example_dupl),
  c("a", "b", "b")))
expect_silent(expect_identical(
  setdiff(x_example_dupl, table_example_dupl),
  c("a", "b")))
expect_silent(expect_identical(
  not_in(x_example_dupl, table_example_dupl, value = FALSE),
  c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)))


#### Tests ####
# 1 (unique and duplicated values present zero to two times in table, return boolean)
expect_silent(expect_identical(
  not_in(x = x, table = table, value = FALSE),
  out_boolean))

# 2 (unique and duplicated values present zero to two times in table, return values)
expect_silent(expect_identical(
  not_in(x = x, table = table, value = TRUE),
  c("only_x_double", "only_x_single", "only_x_double")))

for(value in c(TRUE, FALSE)) {
  ##### Behaviour of NAs #####
  # 3a (numeric NAs are not allowed in 'table')
  expect_error(
    not_in(x = c("a", "b"), table = NA_real_, value = value),
    pattern = "!is.numeric(table) is not TRUE",
    fixed = TRUE)

  # 3b (logical NAs are not allowed in 'x')
  expect_error(
    not_in(x = NA_real_, table = c("a", "b"), value = value),
    pattern = "!is.numeric(x) is not TRUE",
    fixed = TRUE)

  # 3c (NAs match each other, the returned zero-length value has the same type
  # as 'x')
  expect_identical(
    not_in(x = NA_character_, table = NA_character_, value = value),
    if(value) {character(0)} else {FALSE})

  # 3d (NAs of different types in 'x' and 'table' match each other, the returned
  # zero-length value has the same type as 'x')
  expect_identical(
    not_in(x = NA, table = NA_character_, value = value),
    if(value) {logical(0)} else {FALSE})

  expect_identical(
    not_in(x = NA_character_, table = NA, value = value),
    if(value) {character(0)} else {FALSE})

  # 3e (Looking for NA_character_ if it is not present returns NA_character_)
  expect_identical(
    not_in(x = NA_character_, table = c("a", "b"), value = value),
    if(value) {NA_character_} else {TRUE})

  # 3f (non-NA in x, normal character values in table)
  expect_identical(
    not_in(x = c("a", "b"), table = NA_character_, value = value),
    if(value) {c("a", "b")} else {c(TRUE, TRUE)})

  # 3g (zero-length value in x, normal character value in table)
  expect_error(
    not_in(x = character(0), table = c("a", "b"), value = value),
    pattern = "length(x) > 0L is not TRUE", fixed = TRUE)

  # 3h (normal character values in x, zero-length value in table)
  expect_error(
    not_in(x = c("a", "b"), table = character(0), value = value),
    pattern = "length(table) > 0L is not TRUE", fixed = TRUE)

  # 3i (empty quotes behave normally)
  expect_identical(
    not_in(x = "", table = c("a", "b"), value = value),
    if(value) {""} else {TRUE})

  # 3j (empty quotes behave normally)
  expect_identical(
    not_in(x = c("a", "b"), table = "", value = value),
    if(value) {c("a", "b")} else {c(TRUE, TRUE)})

  # 3k (empty quotes behave normally)
  expect_identical(
    not_in(x = c("a", "", "b"), table = "", value = value),
    if(value) {c("a", "b")} else {c(TRUE, FALSE, TRUE)})

  # 3l (empty quotes behave normally)
  expect_identical(
    not_in(x = "", table = "", value = value),
    if(value) {character(0)} else {FALSE})
}

##### Factor input to 'x' or 'table' ####
# Factor input to 'x' is converted to character, factor input to 'table' behaves
# as if it were character input.
expect_silent(expect_identical(
  not_in(x = factor(letters[1:2]), table = factor(letters[2:3]), value = TRUE),
  letters[1]))

expect_silent(expect_identical(
  not_in(x = factor(letters[1:2]), table = factor(letters[2:3]), value = FALSE),
  c(TRUE, FALSE)))

expect_silent(expect_identical(
  not_in(x = factor(letters[1:2]), table = letters[2:3], value = TRUE),
  letters[1]))

expect_silent(expect_identical(
  not_in(x = factor(letters[1:2]), table = letters[2:3], value = FALSE),
  c(TRUE, FALSE)))

expect_silent(expect_identical(
  not_in(x = letters[1:2], table = factor(letters[2:3]), value = TRUE),
  letters[1]))

expect_silent(expect_identical(
  not_in(x = letters[1:2], table = factor(letters[2:3]), value = FALSE),
  c(TRUE, FALSE)))


#### Remove objects used in tests ####
rm(out_boolean, value, x, x_example, x_example_dupl, table, table_example, table_example_dupl)
