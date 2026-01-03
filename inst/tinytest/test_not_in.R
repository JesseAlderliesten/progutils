#### Create objects to use in tests ####
x <- c("shared_both_double", "shared_x_double_y_single",
       "only_x_double", "only_x_single",
       "shared_both_single", "shared_x_single_y_double",
       "only_x_double", "shared_x_double_y_single",
       "shared_both_double")
y <- c("only_y_double", "shared_both_double",
       "shared_x_single_y_double", "shared_both_single",
       "shared_x_single_y_double",
       "shared_x_double_y_single", "shared_both_double",
       "only_y_single", "only_y_double")
out_boolean <- c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)


#### Test the examples ####
x_example <- letters[1:4]
y_example <- letters[3:6]
x_example_dupl <- c(x_example, letters[c(2, 4:6, 5)])
y_example_dupl <- letters[c(3:8, 5:7)]

expect_silent(expect_identical(not_in(x_example, y_example), c("a", "b")))
expect_silent(expect_identical(
  not_in(as.factor(x_example), as.factor(y_example)), c("a", "b")))
expect_silent(expect_identical(
  not_in(x_example, y_example, value = FALSE),
  c(TRUE, TRUE, FALSE, FALSE)))
expect_silent(expect_identical(
  not_in(x_example, y_example, value = FALSE),
  !(x_example %in% y_example)))

expect_silent(expect_identical(
  not_in(x_example_dupl, y_example_dupl),
  c("a", "b", "b")))
expect_silent(expect_identical(
  setdiff(x_example_dupl, y_example_dupl),
  c("a", "b")))
expect_silent(expect_identical(
  not_in(x_example_dupl, y_example_dupl, value = FALSE),
  c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)))


#### Tests ####
# 1 (unique and duplicated values present zero to two times in y, return boolean)
expect_silent(expect_identical(
  not_in(x = x, y = y, value = FALSE),
  out_boolean))

# 2 (unique and duplicated values present zero to two times in y, return values)
expect_silent(expect_identical(
  not_in(x = x, y = y, value = TRUE),
  c("only_x_double", "only_x_single", "only_x_double")))

for(value in c(TRUE, FALSE)) {
  # 3a (NAs of different types match each other, return for 'value = TRUE' has
  # type of 'x')
  expect_identical(
    not_in(x = NA_character_, y = NA, value = value),
    if(value) {character(0)} else {FALSE})

  # 3b (NAs of different types match each other, return for 'value = TRUE' has
  # type of 'x')
  expect_identical(
    not_in(x = NA, y = NA_character_, value = value),
    if(value) {logical(0)} else {FALSE})

  # 3c (NAs of the same type match each other)
  expect_identical(
    not_in(x = NA, y = NA, value = value),
    if(value) {logical(0)} else {FALSE})

  # 3d (Looking for NA if it is not present returns NA)
  expect_identical(
    not_in(x = NA, y = FALSE, value = value),
    if(value) {NA} else {TRUE})
}

# 4a (Factor input to 'x' is converted to character)
expect_silent(expect_identical(
  not_in(x = factor(letters[1:2]), y = factor(letters[2:3]),
         value = TRUE),
  letters[1]))

# 4b (Factor input to 'x' is converted to character)
expect_silent(expect_identical(
  not_in(x = factor(letters[1:2]), y = factor(letters[2:3]),
         value = FALSE),
  c(TRUE, FALSE)))


#### Remove objects used in tests ####
rm(out_boolean, value, x, x_example, x_example_dupl, y, y_example, y_example_dupl)
