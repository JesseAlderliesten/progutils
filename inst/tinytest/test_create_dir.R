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


#### Test the examples ####
my_tempdir <- normalizePath(path = file.path(tempdir(), "testcreatedir"),
                            winslash = "/", mustWork = FALSE)
res_dir_one <- create_dir(dir = file.path(my_tempdir, "dir_one"),
                          add_date = FALSE)
expect_true(dir.exists(res_dir_one))

res_dir_one_v2 <- create_dir(dir = file.path(my_tempdir, "dir_one"),
                             add_date = FALSE)
expect_identical(res_dir_one, res_dir_one_v2)

# On case-insensitive file systems such as Windows and macOS, the created
# directory is the same as 'res_dir_one'. On case-sensitive file systems such as
# Ubuntu, it differs in case from 'res_dir_one'.
expect_silent(create_dir(dir = file.path(my_tempdir, "dir_ONE"),
                         add_date = FALSE))

res_dir_two <- create_dir(dir = file.path(my_tempdir, "dir_two"),
                          add_date = TRUE)
expect_true(dir.exists(res_dir_two))

# Cleaning up
unlink(dirname(res_dir_one), recursive = TRUE)
rm(my_tempdir, res_dir_one, res_dir_one_v2, res_dir_two)


#### Tests ####
my_tempdir <- normalizePath(path = file.path(tempdir(), "testcreatedir"),
                            winslash = "/", mustWork = FALSE)
dir.create(my_tempdir)
my_tempfile <- file.path(my_tempdir, "test_df.csv")
# Write csv-file, modified from example in help(write.table)
write.table(x = data.frame(a = "a", b = pi), file = my_tempfile)


# without date directory
dir <- file.path(my_tempdir, "temp_subdirF_dateF")
expected_path <- dir

expect_silent(expect_false(dir.exists(expected_path)))
expect_silent(
  expect_identical(
    create_dir(dir = dir, add_date = FALSE),
    normalizePath(expected_path, winslash = "/", mustWork = FALSE)))
expect_true(dir.exists(expected_path))

# without date directory, directory already exists
expect_silent(
  expect_identical(
    create_dir(dir = dir, add_date = FALSE),
    normalizePath(expected_path, winslash = "/", mustWork = FALSE)))
if(expect_true(dir.exists(expected_path))) {
  expect_equal(unlink(x = expected_path, recursive = TRUE), 0,
               info = "Check if removing temporary directories was successful")
}

# with date directory
dir <- file.path(my_tempdir, "temp_subdirF_dateT")
expected_path <- file.path(dir, format(Sys.time(), format = "%Y_%m_%d"))

expect_false(dir.exists(expected_path))
expect_silent(
  expect_identical(
    create_dir(dir = dir, add_date = TRUE),
    normalizePath(expected_path, winslash = "/", mustWork = FALSE)))
expect_true(dir.exists(expected_path))

# with date directory, directory already exists
expect_silent(
  expect_identical(
    create_dir(dir = dir, add_date = TRUE),
    normalizePath(expected_path, winslash = "/", mustWork = FALSE)))

# Also recognise that a directory already exists if it has subdirectories
expect_silent(
  expect_identical(
    create_dir(dir = dir),
    normalizePath(expected_path, winslash = "/", mustWork = FALSE)))
if(expect_true(dir.exists(expected_path))) {
  expect_equal(
    unlink(x = dirname(expected_path), recursive = TRUE), 0,
    info = "Check if removing temporary directories was successful")
}

# with subdirectories, with date directory
dir <- file.path(my_tempdir, "temp_subdirT_dateT")
expected_path <- file.path(dir, "subdir", format(Sys.time(), format = "%Y_%m_%d"))

expect_false(dir.exists(expected_path))
expect_silent(
  expect_identical(
    create_dir(dir = file.path(dir, "subdir"), add_date = TRUE),
    normalizePath(expected_path, winslash = "/", mustWork = FALSE)))
expect_true(dir.exists(expected_path))

# with subdirectories, with date directory, directory already exists
expect_silent(
  expect_identical(
    create_dir(dir = file.path(dir, "subdir"), add_date = TRUE),
    normalizePath(expected_path, winslash = "/", mustWork = FALSE)))
if(expect_true(dir.exists(expected_path))) {
  expect_equal(
    unlink(x = dirname(dirname(expected_path)), recursive = TRUE),
    0, info = "Check if removing temporary directories was successful")
}

# Checks on input to 'dir'
for(dir in list(3, "", character(0), NULL, c("temp_p1", "temp_p2"))) {
  expect_error(
    create_dir(dir = dir),
    pattern = "checkinput::is_character(path) is not TRUE", fixed = TRUE)
}

for(dir in list(file.path(my_tempdir, "temp", "."),
                file.path(my_tempdir, "temp."),
                file.path(my_tempdir, "temp.", "subtemp"))) {
  expect_error(
    create_dir(dir = dir),
    pattern = "'dir' should not end with ' ' or '.'", fixed = TRUE)
}

# Need paste0() because file.path() removes trailing slashes.
for(dir in list(paste0(file.path(my_tempdir, "temp"), "//"),
                file.path(paste0(file.path(my_tempdir, "temp"), "/"), "subtemp"),
                file.path(my_tempdir, "\\"),
                file.path(my_tempdir, "temp\\"),
                file.path(my_tempdir, "temp\\", "subtemp"))) {
  expect_error(
    create_dir(dir = dir),
    pattern = "'dir' should not end with '/' or '\\'", fixed = TRUE)
}

# NB. 'dir' equal to '.' can be used to denote the current working directory.
for(dir in list(file.path(my_tempdir, " "),
                file.path(my_tempdir, "temp "),
                file.path(my_tempdir, "temp ", "subtemp"),
                file.path(my_tempdir, "."),
                file.path(my_tempdir, "temp."),
                file.path(my_tempdir, "temp.", "subtemp"))) {
  expect_error(
    create_dir(dir = dir),
    pattern = "'dir' should not end with ' ' or '.'", fixed = TRUE)
}

# 'dir' points to a file instead of a directory
expect_warning(
  expect_equal(create_dir(dir = my_tempfile, add_date = FALSE),
               normalizePath(getwd(), winslash = "/", mustWork = FALSE)),
  pattern = paste0("Returning the working directory instead:\n", getwd()))

# Checks on input to 'add_date'
for(add_date in list(3, NA)) {
  expect_error(
    create_dir(dir = my_tempdir, add_date = add_date),
    pattern = "is_logical(add_date) is not TRUE", fixed = TRUE)
}


#### Delete the created temporary files ####
unlink(my_tempdir, recursive = TRUE)


#### Remove objects used in tests ####
rm(add_date, dir, expected_path, my_tempdir, my_tempfile)
