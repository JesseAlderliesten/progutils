#### Notes ####
# - A temporary directory is used to write the added directories to. The files
#   written to that directory by this script are deleted when they are not
#   needed anymore. Deleting the entire temporary directory would lead to
#   problems because other R-processes write to the same temporary directory.
# - The returned message is only checked for containing 'Created directory' or
#   'already exists' because getting the correct type and number of slashes in
#   the string to compare with the path recorded in the message is brittle.
#   Checking the path is thus left to expect_identical() and
#   expect_true(dir.exists(expected_path)).

tinytest::report_side_effects()


#### Tests ####
my_tempdir <- normalizePath(path = tempdir(), winslash = "/", mustWork = FALSE)

# Possible to create a temporary subdirectory
expect_false(dir.exists(file.path(my_tempdir, "createtempdir")))
res_subdir <- create_tempdir(subdir = "createtempdir")
expect_true(dir.exists(res_subdir))

# Error if temporary subdirectory already exists
expect_error(create_tempdir(subdir = "createtempdir"),
             pattern = paste0("Temporary directory already exists: change",
                              " 'subdir' ('createtempdir')"), fixed = TRUE)

# Temporary subdirectory is writeable
my_tempfile <- file.path(res_subdir, "test_df.csv")
expect_false(file.exists(my_tempfile))
# Write csv-file, modified from example in help(write.table)
write.table(x = data.frame(a = "a", b = pi), file = my_tempfile)
expect_true(file.exists(my_tempfile))

# Target points to a file instead of a directory
expect_warning(
  expect_error(
    create_tempdir(subdir = file.path(basename(dirname(my_tempfile)),
                                      basename(my_tempfile))),
    pattern = "Attempt to create a subdirectory in the temporary directory failed"),
  pattern = "already exists"
)

# Also recognise that a directory already exists if it has subdirectories
expect_false(dir.exists(file.path(my_tempdir, "subdir", "abc")))
res_subdir_recursive <- create_tempdir(subdir = file.path("subdir", "abc"))
expect_true(dir.exists(res_subdir_recursive))

# Checks on input to 'dir'
for(subdir in list(3, "", character(0), NULL, c("temp_p1", "temp_p2"))) {
  expect_error(
    create_tempdir(subdir = subdir),
    pattern = "is_character(subdir) is not TRUE", fixed = TRUE)
}

for(subdir in list("temp_p3/", "temp_p3/", "temp_p4\\")) {
  expect_error(
    create_tempdir(subdir = subdir),
    # NB. need to use '\\' instead of '\' which would test for ''.
    pattern = "should not end with '/' or '\\'",
    fixed = TRUE)
}

for(subdir in list(".")) {
  expect_error(
    create_tempdir(subdir = subdir),
    pattern = "would write to 'tempdir()'", fixed = TRUE)
}

for(subdir in list("..")) {
  expect_error(
    create_tempdir(subdir = subdir),
    pattern = "would write above 'tempdir()'", fixed = TRUE)
}

for(subdir in list("temp_p5.", "temp_p6 ")) {
  expect_error(
    create_tempdir(subdir = subdir),
    pattern = "should not end with ' ' or '.'", fixed = TRUE)
}


#### Delete the created temporary files ####
unlink(c(res_subdir, dirname(res_subdir_recursive)), recursive = TRUE)


#### Remove objects used in tests ####
rm(my_tempdir, my_tempfile, res_subdir, res_subdir_recursive, subdir)
