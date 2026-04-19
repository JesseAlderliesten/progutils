#### To do ####
# Test "" in values, levels, new_order


#### Create objects to use in tests ####
vals <- letters[c(1:3, 2:1)]
input <- factor(x = vals)
new_order <- c("b", "a", "c")
input_missing_levels <- factor(x = vals, levels = letters[1:2])
output_missing_levels <- factor(input_missing_levels,
                                levels = c(new_order[1:2], NA_character_),
                                exclude = NULL)
output <- factor(x = input, levels = new_order)
input_unused_levels <- factor(x = vals, levels = letters[1:4])
input_NA_levels <- factor(x = vals, levels = c(vals[1:2], NA, vals[3]),
                          exclude = NULL)
output_NA_levels <- output
input_NA_levels_val <- input_missing_levels
input_NA_levels_val <- addNA(input_NA_levels_val, ifany = TRUE)
factor_order <- factor(x = new_order, levels = new_order[3:1])
vals_NA <- c(vals[1:3], NA, vals[2:1])

warn_append_levels <- paste0("Appended levels of 'x' that were not present in",
                             " 'new_order' to 'new_order':\n")
warn_drop_levels <- "Dropped levels of 'x' that are not present in its values:\n"
warn_drop_order <- "Dropped values of 'new_order' that are not present in 'x':\n"
warn_missing_levels <- "Levels of 'x' are not present in 'new_order': "
warn_new_order_nonchar <- paste0("checkinput::all_characters(new_order,",
                                 " allow_empty = TRUE, allow_NA = TRUE) is not TRUE")


#### Test the examples ####
vals_example <- letters[c(12:13, 13:11)]
orig <- factor(vals_example, levels = letters[13:11])
expect_silent(
  expect_identical(reorder_levels(x = orig, new_order = letters[11:13]),
                   factor(vals_example, levels = letters[11:13]))
)

levels(orig) <- letters[11:13]
expect_silent(
  expect_identical(orig,
                   factor(letters[c(12:11, 11:13)], levels = letters[11:13]))
)


#### Tests ####
# Normal input
expect_silent(
  expect_identical(reorder_levels(x = input, new_order = new_order), output)
)

# Character input to 'x'
expect_silent(
  expect_identical(
    reorder_levels(x = vals, new_order = new_order),
    output)
)

expect_silent(
  expect_identical(
    reorder_levels(x = vals, new_order = new_order),
    reorder_levels(x = input, new_order = new_order)))

# Character input to 'x' with NA in its values
expect_warning(
  expect_identical(
    reorder_levels(x = vals_NA, new_order = new_order),
    factor(x = vals_NA, levels = c(new_order, NA), exclude = NULL)),
  pattern = paste0(warn_append_levels, "'NA'"),
  strict = TRUE, fixed = TRUE)

# Factor with NA missing from its levels. Testing the warnings separately.
expect_warning(
  expect_identical(
    reorder_levels(x = input_missing_levels, new_order = new_order),
    output_missing_levels),
  pattern = "Added missing levels for values of 'x':\n'NA'",
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_identical(
    reorder_levels(x = input_missing_levels, new_order = new_order),
    output_missing_levels),
  pattern = paste0(warn_append_levels, "'NA'"), strict = TRUE, fixed = TRUE)

# factor with unused levels
expect_warning(
  expect_identical(
    reorder_levels(x = input_unused_levels, new_order = new_order),
    factor(x = input_unused_levels, levels = new_order)),
  pattern = paste0(warn_drop_levels, "'d'"), strict = TRUE, fixed = TRUE)

# factor with unused NA level
expect_warning(
  expect_identical(
    reorder_levels(x = input_NA_levels, new_order = new_order),
    factor(x = input_unused_levels, levels = new_order)),
  pattern = paste0(warn_drop_levels, "'NA'"), strict = TRUE, fixed = TRUE)

# factor with NA in its values and its levels, NA missing from 'new_order'
expect_warning(
  expect_identical(
    reorder_levels(x = input_NA_levels_val, new_order = new_order),
    factor(input_NA_levels_val, levels = c(new_order[1:2], NA), exclude = NULL)),
  pattern = paste0(warn_append_levels, "'NA'"), strict = TRUE, fixed = TRUE)

# some values of 'new_order' missing from 'x'
expect_warning(
  expect_identical(
    reorder_levels(x = input, new_order = c(new_order, "d"),
                   warn_drop_order = TRUE),
    output),
  pattern = paste0(warn_drop_order, "'d'"), strict = TRUE, fixed = TRUE)

expect_silent(
  expect_identical(
    reorder_levels(x = input, new_order = c(new_order, "d"),
                   warn_drop_order = FALSE),
    output)
)

# all values of 'new_order' missing from 'x'
expect_warning(
  expect_identical(reorder_levels(x = input, new_order = letters[4:6]), input),
  pattern = paste0(warn_append_levels, "'a', ", paste_quoted(letters[2:3])),
  strict = TRUE, fixed = TRUE)

expect_error(reorder_levels(x = 1:3, new_order = new_order),
             pattern = "is.factor(x) || is.character(x) is not TRUE", fixed = TRUE)

expect_error(reorder_levels(x = NULL, new_order = new_order),
             pattern = "is.factor(x) || is.character(x) is not TRUE", fixed = TRUE)

expect_error(reorder_levels(x = character(0), new_order = new_order),
             pattern = "length(x) > 0L is not TRUE", fixed = TRUE)

expect_error(reorder_levels(x = factor(), new_order = new_order),
             pattern = "length(x) > 0L is not TRUE", fixed = TRUE)

expect_error(reorder_levels(x = input, new_order = 1:3),
             pattern = warn_new_order_nonchar, fixed = TRUE)

expect_error(reorder_levels(x = input, new_order = NULL),
             pattern = warn_new_order_nonchar, fixed = TRUE)

expect_error(reorder_levels(x = input, new_order = character(0)),
             pattern = warn_new_order_nonchar, fixed = TRUE)

expect_error(reorder_levels(x = input, new_order = factor()),
             pattern = warn_new_order_nonchar, fixed = TRUE)

expect_error(reorder_levels(x = input, new_order = factor_order),
             pattern = warn_new_order_nonchar, fixed = TRUE)

expect_error(reorder_levels(x = input, new_order = c(new_order, new_order[2])),
             pattern = "Values in 'new_order' should be unique", fixed = TRUE)

expect_error(reorder_levels(x = input, new_order = new_order,
                            warn_drop_order = 1),
             pattern = "checkinput::is_logical(warn_drop_order) is not TRUE",
             fixed = TRUE)

expect_error(reorder_levels(x = input, new_order = new_order,
                            warn_drop_order = NA),
             pattern = "checkinput::is_logical(warn_drop_order) is not TRUE",
             fixed = TRUE)

expect_error(reorder_levels(x = input, new_order = new_order,
                            warn_drop_order = "a"),
             pattern = "checkinput::is_logical(warn_drop_order) is not TRUE",
             fixed = TRUE)


#### Remove objects used in tests ####
rm(factor_order, input, input_missing_levels, input_NA_levels,
   input_NA_levels_val, input_unused_levels, new_order, orig, output,
   output_missing_levels, output_NA_levels, vals, vals_example, vals_NA,
   warn_append_levels, warn_drop_levels, warn_drop_order, warn_missing_levels,
   warn_new_order_nonchar)
