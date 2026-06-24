tinytest::report_side_effects()

#### Test the examples ####
basename_my_tempdir <- basename(tempdir(check = TRUE))
prefix <- "subtempdir"

# Create a directory inside the directory returned by 'tempdir()'
my_subtempdir_ex1 <- create_tempdir(prefix = prefix)

# Using the same 'prefix' again creates another directory
my_subtempdir_ex2 <- create_tempdir(prefix = prefix)

# Test that directories exist
expect_true(fs::dir_exists(my_subtempdir_ex1))
expect_true(fs::dir_exists(my_subtempdir_ex2))

# Test that directories are inside 'tempdir()'
expect_identical(basename(dirname(my_subtempdir_ex1)), basename_my_tempdir)
expect_identical(basename(dirname(my_subtempdir_ex2)), basename_my_tempdir)

# Test that names start with 'prefix'
expect_true(startsWith(x = basename(my_subtempdir_ex1), prefix = prefix))
expect_true(startsWith(x = basename(my_subtempdir_ex2), prefix = prefix))

# Test that names are not equal to 'prefix'
expect_true(basename(my_subtempdir_ex1) != prefix)
expect_true(basename(my_subtempdir_ex2) != prefix)

# Test that names are not the same
expect_true(my_subtempdir_ex1 != my_subtempdir_ex2)

# Test that it is not possible to create recursive directories
expect_error(
  create_tempdir(prefix = "subtempdir/otherdir"),
  pattern = "'prefix' should not include file separators", fixed = TRUE)

expect_error(
  create_tempdir(prefix = "subtempdir\\otherdir"),
  pattern = "'prefix' should not include file separators", fixed = TRUE)

#### Delete the created temporary files ####
unlink(c(my_subtempdir_ex1, my_subtempdir_ex2), recursive = TRUE)
expect_false(fs::dir_exists(my_subtempdir_ex1))
expect_false(fs::dir_exists(my_subtempdir_ex2))

#### Remove objects used in tests ####
rm(my_subtempdir_ex1, my_subtempdir_ex2, basename_my_tempdir, prefix)


#### Tests ####
prefix <- "subtempdir"
my_subtempdir_t1 <- create_tempdir()
my_tempfile <- fs::path(my_subtempdir_t1, "test_df.csv")
my_subtempdir_t2 <- create_tempdir(prefix = ".")

# Test that temporary subdirectory is writeable by writing a csv-file, modified
# from an example in help(write.table)
expect_false(fs::is_file(my_tempfile))
write.table(x = data.frame(a = "a", b = pi), file = my_tempfile)
expect_true(fs::is_file(my_tempfile))

# Test that prefix '"."' is accepted
expect_true(fs::dir_exists(my_subtempdir_t2))

#### Delete the created temporary files ####
unlink(c(my_subtempdir_t1, my_subtempdir_t2), recursive = TRUE)

#### Remove objects used in tests ####
rm(my_tempfile, prefix, my_subtempdir_t1, my_subtempdir_t2)


#### Invalid input ####
for(prefix in list(3, NULL, character(0), "", c("temp_p1", "temp_p2"))) {
    expect_error(
      create_tempdir(prefix = prefix),
      pattern = "'prefix' should be a non-empty, non-NA_character_ character string",
      fixed = TRUE)
}

for(prefix in c("tem\\p_p1", "temp_p2\\", "tem/p_p3", "temp_p4/", "\\temp_p5",
                 "/temp_p6")) {
  expect_error(
    create_tempdir(prefix = prefix),
    pattern = "'prefix' should not include file separators", fixed = TRUE)
}

expect_warning(
  expect_error(
    create_tempdir(prefix = "abc|def"),
    pattern = "checkinput::is_path(tempdir_target) is not TRUE",
    fixed = TRUE),
  pattern = "'tempdir_target' should not contain '\"', '*', '?', '|', '<' or '>'",
  strict = TRUE, fixed = TRUE)


#### Remove objects used in tests ####
rm(prefix)
