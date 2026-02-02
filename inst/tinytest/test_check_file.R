#### Test the examples ####
my_tempfiles <- tempfile(pattern = c("FirstFile", "SecondFile"), fileext = ".txt")
# Create the files
expect_silent(
  expect_equal(file.create(my_tempfiles), rep(TRUE, length(my_tempfiles)))
)

expect_message(
  expect_equal(check_file(dir = tempdir(), pattern = "First"),
               grep(pattern = "First", x = basename(my_tempfiles), value = TRUE,
                    fixed = TRUE)),
  pattern = "Using file", strict = TRUE, fixed = TRUE)

# The same file is found if case-insensitive matching is used:
expect_message(
  expect_equal(check_file(dir = tempdir(), pattern = "FIRST", ignore_case = TRUE),
               grep(pattern = "First", x = basename(my_tempfiles), value = TRUE,
                    fixed = TRUE)),
  pattern = "Using file", strict = TRUE, fixed = TRUE)

expect_error(
  check_file(dir = tempdir(), pattern = "FIRST", ignore_case = FALSE),
  pattern = "However, a case-insensitive\nmatch to 'pattern' is present"
)

expect_error(
  check_file(dir = tempdir(), pattern = "abcde", ignore_case = TRUE),
  pattern = "No case-insensitive matches to pattern 'abcde' are present"
)

expect_error(
  check_file(dir = tempdir(), pattern = "abcde", ignore_case = FALSE),
  pattern = "No case-sensitive matches to pattern 'abcde' are present"
)

expect_error(
  check_file(dir = tempdir(), pattern = "abcde", ignore_case = FALSE),
  pattern = "No case-sensitive matches to pattern 'abcde' are present"
)

expect_error(
  check_file(dir = tempdir(), pattern = "File"),
  pattern = "Multiple case-insensitive matches to pattern 'File' are present"
)

# Deleting the created temporary files
unlink(x = my_tempfiles)
rm(my_tempfiles)


#### Tests ####
# Create files in a temporary directory so we know what is present.
my_tempdir <- tempdir()
my_tempfile <- file.path(my_tempdir, "test_df.csv")
# Write csv-file, modified from example in help(write.table)
write.table(x = data.frame(a = "a", b = pi), file = my_tempfile)

# 'dir' points to a file instead of a directory
expect_error(check_file(dir = my_tempfile, pattern = "test_"),
             pattern = "points to a file but should point to a directory")

# 'dir' points to a non-existing directory
expect_error(check_file(dir = file.path(my_tempdir, "abc"), pattern = "test_"),
             pattern = "does not exist")

# 'pattern' points to an existing directory instead of to an existing file
dir.create(path = file.path(tempdir(), "test_dir"), recursive = TRUE)
expect_error(check_file(dir = my_tempdir, pattern = "test_dir"),
             pattern = "No case-insensitive matches to pattern 'test_dir' are present",
             fixed = TRUE)


#### Delete the created temporary files ####
unlink(x = c(my_tempfile, file.path(tempdir(), "test_dir")), recursive = TRUE)


#### Remove objects used in tests ####
rm(my_tempdir, my_tempfile)
