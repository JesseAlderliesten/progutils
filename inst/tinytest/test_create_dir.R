#### To do ####
# - tinytest supports tracking of side-effects, but I'm not yet sure how to
#   properly use it. E.g.,
#   m <- tinytest::run_test_file(file = "inst/tinytest/test_create_dir.R",
#                                side_effects = TRUE)
#   tinytest::run_test_file(file = "inst/tinytest/test_create_dir.R",
#                           side_effects = report_side_effects(
#                             report = FALSE, envvar = FALSE, pwd = TRUE,
#                             files = TRUE, locale = FALSE))
#   tinytest::report_side_effects(report = FALSE)
# - Create test files in a temporary folder that is (automatically or
#   programmatically) removed after the test is run. See ?tinytest::run_test_dir(),
#   ?tinytest::run_test_file(), ?tempdir, ?tempfile, ?unlink, ?files,
#   https://github.com/r-lib/testthat/issues/664, and
#   https://stackoverflow.com/questions/17107206/change-temporary-directory
#   And the code in https://stat.ethz.ch/pipermail/r-package-devel/2024q4/011196.html:
#   "I think the idea is to do as below. You can also make temporary directories
#   in a similar way."
#     tempFile <- tempfile()
#     # write to tempFile, and use it anyway you like
#     unlink(tempFile)

#### Test the examples ####
warning("Not yet tested examples of create_dir()!")


#### Tests ####
# 1a without date directory
dir <- "temp_subdirF_dateF"
expected_path <- dir
expect_message(
  expect_identical(
    create_dir(dir = dir, add_date = FALSE),
    expected_path),
  pattern = paste0("Created directory.+", expected_path), strict = TRUE)
expect_true(dir.exists(expected_path))

# 1b without date directory, directory already exists
expect_message(
  expect_identical(
    create_dir(dir = dir, add_date = FALSE),
    expected_path),
  pattern = paste0(expected_path, ".+already exists"), strict = TRUE)
expect_true(dir.exists(expected_path))

# 2a with date directory
dir <- "temp_subdirF_dateT"
expected_path <- file.path(dir, format(Sys.time(), format = "%Y_%m_%d"))
expect_message(
  expect_identical(
    create_dir(dir = dir, add_date = TRUE),
    expected_path),
  pattern = paste0("Created directory.+", expected_path), strict = TRUE)
expect_true(dir.exists(expected_path))

# 2b with date directory, directory already exists
expect_message(
  expect_identical(
    create_dir(dir = dir, add_date = TRUE),
    expected_path),
  pattern = paste0(expected_path, ".+already exists"), strict = TRUE)
expect_true(dir.exists(expected_path))

# 3a with subdirectories, with date directory
dir <- "temp_subdirT_dateT"
expected_path <- file.path(dir, "subdir", format(Sys.time(), format = "%Y_%m_%d"))
expect_message(
  expect_identical(
    create_dir(dir = file.path(dir, "subdir"), add_date = TRUE),
    expected_path),
  pattern = paste0("Created directory.+", expected_path), strict = TRUE)
expect_true(dir.exists(expected_path))

# 3b with subdirectories, with date directory, directory already exists
expect_message(
  expect_identical(
    create_dir(dir = file.path(dir, "subdir"), add_date = TRUE),
    expected_path),
  pattern = paste0(expected_path, ".+already exists"), strict = TRUE)
expect_true(dir.exists(expected_path))

# 4a "." should create the subfolder with the current date in the working directory
dir <- "."
expected_path <- paste0(dir, .Platform$file.sep,
                        format(Sys.time(), format = "%Y_%m_%d"))
expect_message(
  expect_identical(
    create_dir(dir = dir, add_date = TRUE),
    expected_path),
  pattern = paste0("Created directory '", expected_path), strict = TRUE)
expect_true(dir.exists(file.path(expected_path)))
expect_true(dir.exists(file.path(getwd(), format(Sys.time(), format = "%Y_%m_%d"))))

# 4b further checks on input to 'dir'
for(dir in list(3, "", character(0), NULL, c("temp_p1", "temp_p2"))) {
  expect_error(
    create_dir(dir = dir),
    pattern = "is_character(dir) is not TRUE", fixed = TRUE)
}

for(dir in list("./", "temp_p1/")) {
  expect_error(
    create_dir(dir = dir),
    pattern = "'dir' should not end with '/'", fixed = TRUE)
}

for(dir in list(".\\", "temp_p1\\")) {
  expect_error(
    create_dir(dir = dir),
    pattern = "'dir' should not end with '\\'", fixed = TRUE)
}

# 5 Checks on input to 'add_date'
for(add_date in list(3, NA)) {
  expect_error(
    create_dir(dir = ".", add_date = add_date),
    pattern = "is_logical(add_date) is not TRUE", fixed = TRUE)
}

# 6 Checks on input to 'quietly'
for(quietly in list(3, NA)) {
  expect_error(
    create_dir(dir = ".", quietly = quietly),
    pattern = "is_logical(quietly) is not TRUE", fixed = TRUE)
}


#### Remove objects used in tests ####
rm(add_date, dir, expected_path, quietly)
