#### To do ####
# tinytest supports tracking of side-effects, but I'm not yet sure how to
# properly use it. E.g.,
# m <- tinytest::run_test_file(file = "inst/tinytest/test_create_dir.R",
#                              side_effects = TRUE)
# tinytest::run_test_file(file = "inst/tinytest/test_create_dir.R",
#   side_effects = report_side_effects(report = FALSE, envvar = FALSE, pwd = TRUE,
#                                      files = TRUE, locale = FALSE))
# tinytest::report_side_effects(report = FALSE)

# Test creation of files in a temporary folder that is automatically destroyed
# after the test is run. See the examples in ?run_test_dir() and ?run_test_file()
# and see https://stackoverflow.com/questions/17107206/change-temporary-directory


#### Create objects to use in tests ####
warning("Not yet implemented all tests for create_dir()!")

#### Test the examples ####


#### Test other sections ####

#### Tests ####

# 1a no subdirectory, with date directory
dir <- "temp_subdirF_dateT"
expected_path <- file.path(dir, format(Sys.time(), format = "%Y_%m_%d"))
expect_message(
  expect_identical(
    create_dir(dir = dir, subdir = "", add_date = TRUE),
    expected_path),
  pattern = paste0("Created directory.+", expected_path), strict = TRUE)
expect_true(dir.exists(expected_path))

# 1b no subdirectory, with date directory, directory already exists
expect_message(
  expect_identical(
    create_dir(dir = dir, subdir = "", add_date = TRUE),
    expected_path),
  pattern = paste0(expected_path, ".+already exists"), strict = TRUE)
expect_true(dir.exists(expected_path))

# 2a with subdirectory, with date directory
dir <- "temp_subdirT_dateT"
expected_path <- file.path(dir, "subdir", format(Sys.time(), format = "%Y_%m_%d"))
expect_message(
  expect_identical(
    create_dir(dir = dir, subdir = "subdir", add_date = TRUE),
    expected_path),
  pattern = paste0("Created directory.+", expected_path), strict = TRUE)
expect_true(dir.exists(expected_path))

# 2b with subdirectory, with date directory, directory already exists
expect_message(
  expect_identical(
    create_dir(dir = dir, subdir = "subdir", add_date = TRUE),
    expected_path),
  pattern = paste0(expected_path, ".+already exists"), strict = TRUE)
expect_true(dir.exists(expected_path))

# 2c 'dir' and 'subdir' contains slashes to indicate subdirectories, with date
# directory
dir <- file.path("temp_dirsubs_subdirT_dateT", "sub1", "sub2")
expected_path <- file.path(dir, "subdir", "sub3", "sub4",
                           format(Sys.time(), format = "%Y_%m_%d"))
expect_message(
  expect_identical(
    create_dir(dir = dir, subdir = file.path("subdir", "sub3", "sub4"),
               add_date = TRUE),
    expected_path),
  pattern = paste0("Created directory.+", expected_path), strict = TRUE)
expect_true(dir.exists(expected_path))

# 3a checks on input to 'dir'
for(dir in list(3, "", character(0), NULL, c("temp_p1", "temp_p2"))) {
  expect_error(
    create_dir(dir = dir, subdir = ""),
    pattern = "is_character(dir) is not TRUE", fixed = TRUE)
}

# 3b checks on input to 'subdir'
for(subdir in list(3, character(0), NULL, c("temp_p1", "temp_p2"))) {
  expect_error(
    create_dir(dir = ".", subdir = subdir),
    pattern = "is_character(subdir, allow_empty = TRUE) is not TRUE", fixed = TRUE)
}

# 4a further checks on input to 'dir'
for(dir in list("./", ".\\", "temp_p1/", "temp_p1\\")) {
  expect_error(
    create_dir(dir = dir, subdir = ""),
    pattern = "is_character(dir) is not TRUE", fixed = TRUE)
}

# 4b further checks on input to 'subdir'
for(subdir in list(character(0), NULL, c("temp_p1", "temp_p2"))) {
  expect_error(
    create_dir(dir = ".", subdir = subdir),
    pattern = "is_character(subdir, allow_empty = TRUE) is not TRUE", fixed = TRUE)
}




#### Remove objects used in tests ####
