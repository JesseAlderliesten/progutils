#### Create objects to use in tests ####
test_df <- data.frame(a = 1:2, b = 11:12, c = 21:22)
test_mat <- as.matrix(test_df)
new_order <- c("b", "a", "c")

test_df_weird <- test_df
test_df_weird <- cbind(test_df_weird, d = 31:32)
colnames(test_df_weird)[3] <- NA_character_
colnames(test_df_weird)[4] <- ""
test_mat_weird <- as.matrix(test_df_weird)
new_order_weird <- c("", "b", "a", NA_character_, "d")

output_df <- test_df[, new_order]
output_mat <- test_mat[, new_order]

warn_new_order <- paste0("checkinput::all_characters(new_order, allow_empty =",
                         " TRUE, allow_NA = TRUE) is not TRUE")


#### Test the examples ####
expect_silent(
  expect_identical(reorder_cols(x = test_df, new_order = new_order), output_df)
)

expect_warning(
  expect_identical(reorder_cols(x = test_df, new_order = c("b", "a", "d")),
                   output_df),
  pattern = paste0("Appended columns that are present in 'x' but missing from",
                   " 'new_order':\n'c'"),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_identical(reorder_cols(x = test_df, new_order = c("b", "a", "d")),
                   output_df),
  pattern = paste0("Dropped values of 'new_order' that are not present in",
                   " column names of 'x':\n'd'"),
  strict = TRUE, fixed = TRUE)


#### Tests ####
# Normal data.frame input
expect_silent(
  expect_identical(reorder_cols(x = test_df, new_order = new_order), output_df)
)

# Weird data.frame input
expect_warning(
  expect_error(
    reorder_cols(x = test_df_weird, new_order = new_order),
    pattern = "Column names of 'x' should be unique and syntactically valid", fixed = TRUE),
  pattern = "Names are syntactically invalid: 'NA', '\"\"'", strict = TRUE, fixed = TRUE)

# data.frame input without column names
expect_error(reorder_cols(x = unname(test_df), new_order = new_order),
             pattern = "'x' should have column names", fixed = TRUE)

# Normal matrix input
expect_silent(
  expect_identical(reorder_cols(x = test_mat, new_order = new_order), output_mat)
)

# Weird matrix input
expect_warning(
  expect_error(
    reorder_cols(x = test_mat_weird, new_order = new_order),
    pattern = "Column names of 'x' should be unique and syntactically valid", fixed = TRUE),
  pattern = "Names are syntactically invalid: 'NA', '\"\"'", strict = TRUE, fixed = TRUE)

# matrix input without column names
expect_error(reorder_cols(x = unname(test_mat), new_order = new_order),
             pattern = "'x' should have column names", fixed = TRUE)

# vector
expect_error(reorder_cols(x = test_df[, 1L, drop = TRUE], new_order = new_order),
             pattern = "'x' should have columns", fixed = TRUE)

# data.frame with zero columns
expect_error(reorder_cols(x = test_df[, 0L, drop = TRUE], new_order = new_order),
             pattern = "'x' should have at least one column", fixed = TRUE)

# matrix with zero columns
expect_error(reorder_cols(x = test_mat[, 0L, drop = TRUE], new_order = new_order),
             pattern = "'x' should have at least one column", fixed = TRUE)

# Zero-length input to 'x'
expect_error(reorder_cols(x = character(0), new_order = new_order),
             pattern = "'x' should have columns", fixed = TRUE)

# Weird input to 'new_order', data.frame input to 'x'.
expect_warning(
  expect_identical(reorder_cols(x = test_df, new_order = new_order_weird),
                   output_df),
  pattern = paste0("Appended columns that are present in 'x' but missing from",
                   " 'new_order':\n'c'"),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_identical(reorder_cols(x = test_df, new_order = new_order_weird),
                   output_df),
  pattern = paste0("Dropped values of 'new_order' that are not present in",
                   " column names of 'x':\n'', 'NA', 'd'"),
  strict = TRUE, fixed = TRUE)

# Weird input to 'new_order', matrix input to 'x'.
expect_warning(
  expect_identical(reorder_cols(x = test_mat, new_order = new_order_weird),
                   output_mat),
  pattern = paste0("Appended columns that are present in 'x' but missing from",
                   " 'new_order':\n'c'"),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_identical(reorder_cols(x = test_mat, new_order = new_order_weird),
                   output_mat),
  pattern = paste0("Dropped values of 'new_order' that are not present in",
                   " column names of 'x':\n'', 'NA', 'd'"),
  strict = TRUE, fixed = TRUE)

# Factor input to 'new_order'
expect_error(reorder_cols(x = test_df, new_order = as.factor(new_order)),
             pattern = warn_new_order, fixed = TRUE)

# Numeric input to 'new_order'
expect_error(reorder_cols(x = test_df, new_order = 1:3),
             pattern = warn_new_order, fixed = TRUE)

# Zero-length input to 'new_order'
expect_error(reorder_cols(x = test_df, new_order = character(0)),
             pattern = warn_new_order, fixed = TRUE)

# Duplicated input to 'new_order'
expect_error(reorder_cols(x = test_df, new_order = c(new_order, new_order[2])),
             pattern = "Values in 'new_order' should be unique", fixed = TRUE)


#### Remove objects used in tests ####
rm(new_order, new_order_weird, output_df, output_mat, test_df, test_df_weird,
   test_mat, test_mat_weird, warn_new_order)
