tinytest::report_side_effects()


#### Test the examples ####
my_tempfiles <- fs::path_abs(
  tempfile(pattern = c("some_filename", "another_filename"), fileext = ".txt")
  )

# Create the files
expect_silent(
  expect_equal(file.create(my_tempfiles), rep.int(TRUE, length(my_tempfiles)))
)

expect_message(
  expect_equal(basename(get_file_path(dir = tempdir(), pattern = "some_filename")),
               basename(grep(pattern = "some_filename", x = my_tempfiles,
                    value = TRUE, fixed = TRUE))),
  pattern = "Using file", strict = TRUE, fixed = TRUE)

# The same file is found if case-insensitive matching is used:
expect_message(
  expect_equal(basename(get_file_path(dir = tempdir(), pattern = "SOME_FILE",
                            ignore_case = TRUE)),
               grep(pattern = "some_file", x = basename(my_tempfiles),
                    value = TRUE, fixed = TRUE)),
  pattern = "Using file", strict = TRUE, fixed = TRUE)

expect_error(
  get_file_path(dir = tempdir(), pattern = "SOME_FILE", ignore_case = FALSE),
  pattern = basename(my_tempfiles[1])
)

expect_error(
  get_file_path(dir = tempdir(), pattern = "missing_filename_abcde",
               ignore_case = TRUE),
  pattern = "No matches to pattern 'missing_filename_abcde' are present"
)

expect_error(
  get_file_path(dir = tempdir(), pattern = "missing_filename_abcde",
               ignore_case = FALSE),
  pattern = "No case-sensitive matches to pattern 'missing_filename_abcde' are present"
)

expect_error(
  get_file_path(dir = tempdir(), pattern = "missing_filename_abcde", ignore_case = FALSE),
  pattern = "No case-sensitive matches to pattern 'missing_filename_abcde' are present"
)

expect_error(
  get_file_path(dir = tempdir(), pattern = "_filename", ignore_case = TRUE),
  pattern = "Multiple matches to pattern '_filename' are present"
)

expect_error(
  get_file_path(dir = tempdir(), pattern = "_filename", ignore_case = FALSE),
  pattern = "Multiple case-sensitive matches to pattern '_filename' are present"
)

# Deleting the created temporary files
unlink(x = my_tempfiles)
rm(my_tempfiles)


#### Tests ####
# Create files in a temporary directory so we know what is present.
my_tempdir <- fs::path(tempdir(), "testgetfilename")
dir.create(my_tempdir)
my_tempfile <- fs::path(my_tempdir, "test_df.csv")

# Write csv-file, modified from example in help(write.table)
write.table(x = data.frame(a = "a", b = pi), file = my_tempfile)

expect_message(
  expect_true(endsWith(
    get_file_path(dir = dirname(my_tempfile), pattern = "test_df",
                  quietly = FALSE),
    suffix = fs::path("testgetfilename", "test_df.csv")
    )),
  pattern = "Using file", strict = TRUE, fixed = TRUE)

expect_silent(
  expect_true(endsWith(
    get_file_path(dir = dirname(my_tempfile), pattern = "test_df",
                  quietly = TRUE),
    suffix = fs::path("testgetfilename", "test_df.csv")
  ))
)

# 'dir' points to a file instead of a directory
expect_error(get_file_path(dir = my_tempfile, pattern = "test_"),
             pattern = "Directory does not exist")

# 'dir' points to a non-existing directory
expect_error(get_file_path(dir = fs::path(my_tempdir, "abc"), pattern = "test_"),
             pattern = "Directory does not exist")

# 'pattern' points to an existing directory instead of to an existing file
dir.create(path = fs::path(tempdir(), "test_dir", "abc"), recursive = TRUE)
expect_error(get_file_path(dir = fs::path(tempdir(), "test_dir"), pattern = "abc"),
             pattern = "No matches to pattern 'abc' are present",
             fixed = TRUE)

##### Invalid input #####
expect_error(get_file_path(dir = "", pattern = "test_dir"),
             pattern = "checkinput::is_character(dir) is not TRUE",
             fixed = TRUE)

expect_error(get_file_path(dir = my_tempdir, pattern = ""),
             pattern = "checkinput::is_character(pattern) is not TRUE",
             fixed = TRUE)

expect_error(get_file_path(dir = my_tempdir, pattern = "test_dir", quietly = 1),
             pattern = "checkinput::is_logical(quietly) is not TRUE",
             fixed = TRUE)


#### Delete the created temporary files ####
unlink(x = c(dirname(my_tempfile), fs::path(tempdir(), "test_dir")),
       recursive = TRUE)


#### Remove objects used in tests ####
rm(my_tempdir, my_tempfile)
