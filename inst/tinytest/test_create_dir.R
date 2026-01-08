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


#### Wishlist ####
# - tinytest supports tracking of side-effects, but I'm not yet sure how to
#   properly use it. E.g.,
#   m <- tinytest::run_test_file(file = "inst/tinytest/test_create_dir.R",
#                                side_effects = TRUE)
#   tinytest::run_test_file(file = "inst/tinytest/test_create_dir.R",
#                           side_effects = report_side_effects(
#                             report = FALSE, envvar = FALSE, pwd = TRUE,
#                             files = TRUE, locale = FALSE))
#   tinytest::report_side_effects(report = FALSE)


#### Test the examples ####
my_tempdir <- tempdir()
expect_message(res_dir_one <- create_dir(dir = file.path(my_tempdir, "dir_one"),
                                         add_date = FALSE),
               pattern = "Created directory", strict = TRUE, fixed = TRUE)
expect_true(dir.exists(res_dir_one))

expect_message(res_dir_one_v2 <- create_dir(dir = file.path(my_tempdir, "dir_one"),
                             add_date = FALSE),
               pattern = "already exists", strict = TRUE, fixed = TRUE)
expect_identical(res_dir_one, res_dir_one_v2)

expect_message(res_dir_two <- create_dir(dir = file.path(my_tempdir, "dir_two"),
                          add_date = TRUE),
               pattern = "Created directory", strict = TRUE, fixed = TRUE)
expect_true(dir.exists(res_dir_two))

# Cleaning up
unlink(c(res_dir_one, dirname(res_dir_two)), recursive = TRUE)
rm(my_tempdir, res_dir_one, res_dir_one_v2, res_dir_two)


#### Tests ####
my_tempdir <- tempdir()

# 1a without date directory
dir <- file.path(my_tempdir, "temp_subdirF_dateF")
expected_path <- dir

expect_silent(expect_false(dir.exists(expected_path)))
expect_message(
  expect_identical(
    create_dir(dir = dir, add_date = FALSE),
    normalizePath(expected_path, mustWork = FALSE)),
  pattern = "Created directory", strict = TRUE, fixed = TRUE)
expect_true(dir.exists(expected_path))

# 1b without date directory, directory already exists
expect_message(
  expect_identical(
    create_dir(dir = dir, add_date = FALSE),
    normalizePath(expected_path, mustWork = FALSE)),
  pattern = "already exists", strict = TRUE, fixed = TRUE)
if(expect_true(dir.exists(expected_path))) {
  expect_equal(unlink(x = expected_path, recursive = TRUE), 0,
               info = "Check if removing temporary directories was successful")
}

# 2a with date directory
dir <- file.path(my_tempdir, "temp_subdirF_dateT")
expected_path <- file.path(dir, format(Sys.time(), format = "%Y_%m_%d"))

expect_false(dir.exists(expected_path))
expect_message(
  expect_identical(
    create_dir(dir = dir, add_date = TRUE),
    normalizePath(expected_path, mustWork = FALSE)),
  pattern = "Created directory", strict = TRUE, fixed = TRUE)
expect_true(dir.exists(expected_path))

# 2b with date directory, directory already exists
expect_message(
  expect_identical(
    create_dir(dir = dir, add_date = TRUE),
    normalizePath(expected_path, mustWork = FALSE)),
  pattern = "already exists", strict = TRUE, fixed = TRUE)

# Also recognise that a directory already exists if it has subdirectories
expect_message(
  expect_identical(
    create_dir(dir = dir),
    normalizePath(expected_path, mustWork = FALSE)),
  pattern = "already exists", strict = TRUE, fixed = TRUE)
if(expect_true(dir.exists(expected_path))) {
  expect_equal(
    unlink(x = dirname(expected_path), recursive = TRUE), 0,
    info = "Check if removing temporary directories was successful")
}

# 3a with subdirectories, with date directory
dir <- file.path(my_tempdir, "temp_subdirT_dateT")
expected_path <- file.path(dir, "subdir", format(Sys.time(), format = "%Y_%m_%d"))

expect_false(dir.exists(expected_path))
expect_message(
  expect_identical(
    create_dir(dir = file.path(dir, "subdir"), add_date = TRUE),
    normalizePath(expected_path, mustWork = FALSE)),
  pattern = "Created directory", strict = TRUE, fixed = TRUE)
expect_true(dir.exists(expected_path))

# 3b with subdirectories, with date directory, directory already exists
expect_message(
  expect_identical(
    create_dir(dir = file.path(dir, "subdir"), add_date = TRUE),
    normalizePath(expected_path, mustWork = FALSE)),
  pattern = "already exists", strict = TRUE, fixed = TRUE)
if(expect_true(dir.exists(expected_path))) {
  expect_equal(
    unlink(x = dirname(dirname(expected_path)), recursive = TRUE),
    0, info = "Check if removing temporary directories was successful")
}

# 4 "." should create the subfolder with the current date in the working directory
dir <- file.path(my_tempdir, ".")
expected_path <- paste0(dir, .Platform$file.sep,
                        format(Sys.time(), format = "%Y_%m_%d"))

expect_false(dir.exists(expected_path))
expect_message(
  expect_identical(
    create_dir(dir = dir, add_date = TRUE),
    normalizePath(expected_path, mustWork = FALSE)),
  pattern = "Created directory", strict = TRUE, fixed = TRUE)
if(expect_true(dir.exists(file.path(expected_path)))) {
  expect_equal(unlink(x = expected_path, recursive = TRUE), 0,
               info = "Check if removing temporary directories was successful")
}

# 5 Checks on input to 'dir'
for(dir in list(3, "", character(0), NULL, c("temp_p1", "temp_p2"))) {
  expect_error(
    create_dir(dir = dir),
    pattern = "is_character(dir) is not TRUE", fixed = TRUE)
}

for(dir in list(paste0(my_tempdir, "./"), paste0(my_tempdir, "temp_p1/"))) {
  expect_error(
    create_dir(dir = dir),
    pattern = "'dir' should not end with '/'", fixed = TRUE)
}

for(dir in list(paste0(my_tempdir, ".\\"), paste0(my_tempdir, "temp_p1\\"))) {
  expect_error(
    create_dir(dir = dir),
    pattern = "'dir' should not end with '\\'", fixed = TRUE)
}

# 6 Checks on input to 'add_date'
for(add_date in list(3, NA)) {
  expect_error(
    create_dir(dir = my_tempdir, add_date = add_date),
    pattern = "is_logical(add_date) is not TRUE", fixed = TRUE)
}

# 7 Checks on input to 'quietly'
for(quietly in list(3, NA)) {
  expect_error(
    create_dir(dir = my_tempdir, quietly = quietly),
    pattern = "is_logical(quietly) is not TRUE", fixed = TRUE)
}


#### Remove objects used in tests ####
rm(add_date, dir, expected_path, my_tempdir, quietly)
