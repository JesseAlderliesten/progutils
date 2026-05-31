tinytest::report_side_effects()


#### Test the examples ####
my_tempdir <- normalizePath(path = fs::path(tempdir(), "testcreatedir"),
                            winslash = "/", mustWork = FALSE)
tempdir_basename <- basename(tempdir())

res_dir_one <- create_dir(dir = fs::path(my_tempdir, "dir_one"),
                          add_date = FALSE)
expect_true(dir.exists(res_dir_one))

res_dir_one_v2 <- create_dir(dir = fs::path(my_tempdir, "dir_one"),
                             add_date = FALSE)
expect_true(endsWith(
  res_dir_one,
  suffix = fs::path(tempdir_basename, "testcreatedir", "dir_one")
))
expect_true(endsWith(
  res_dir_one_v2,
  suffix = fs::path(tempdir_basename, "testcreatedir", "dir_one")
))

# On case-insensitive file systems such as Windows and macOS, the directory
# created below is the same as 'res_dir_one'. On case-sensitive file systems
# such as Ubuntu, it differs in case from 'res_dir_one'.
# Notes:
# - Issues a spurious warning on MacOS ('Repeated '/' or '\\' in 'dir'') because
#   there the part before the output of 'tempdir()' contains repeated slashes.
create_dir(dir = fs::path(my_tempdir, "dir_ONE"), add_date = FALSE)

res_dir_two <- create_dir(dir = fs::path(my_tempdir, "dir_two"),
                          add_date = TRUE)
expect_true(dir.exists(res_dir_two))

# Cleaning up
unlink(dirname(res_dir_one), recursive = TRUE)
rm(my_tempdir, res_dir_one, res_dir_one_v2, res_dir_two, tempdir_basename)


#### Tests ####
my_tempdir <- normalizePath(path = fs::path(tempdir(), "testcreatedir"),
                            winslash = "/", mustWork = FALSE)
dir.create(my_tempdir)
tempdir_basename <- basename(tempdir())
my_tempfile <- fs::path(my_tempdir, "test_df.csv")
# Write csv-file, modified from example in help(write.table)
write.table(x = data.frame(a = "a", b = pi), file = my_tempfile)

# without date directory
dir <- fs::path(my_tempdir, "temp_subdirF_dateF")
expected_path <- dir

expect_false(dir.exists(expected_path))
dir_no_date <- create_dir(dir = dir, add_date = FALSE)
expect_true(dir.exists(expected_path))
expect_true(endsWith(
  dir_no_date,
  suffix = fs::path(tempdir_basename, "testcreatedir", "temp_subdirF_dateF")
))

# without date directory, directory already exists
dir_no_date_v2 <- create_dir(dir = dir, add_date = FALSE)
expect_true(endsWith(
  dir_no_date_v2,
  suffix = fs::path(tempdir_basename, "testcreatedir", "temp_subdirF_dateF")
))

# with date directory
dir <- fs::path(my_tempdir, "temp_subdirF_dateT")
expected_path <- fs::path(dir, format(Sys.time(), format = "%Y_%m_%d"))

expect_false(dir.exists(expected_path))
dir_date <- create_dir(dir = dir, add_date = TRUE)
expect_true(dir.exists(expected_path))
expect_true(endsWith(
  dir_date,
  suffix = fs::path(tempdir_basename, "testcreatedir", "temp_subdirF_dateT",
                     format(Sys.time(), format = "%Y_%m_%d"))
))

# with subdirectories, with date directory
dir <- fs::path(my_tempdir, "temp_subdirT_dateT")
expected_path <- fs::path(dir, "subdir", format(Sys.time(), format = "%Y_%m_%d"))

expect_false(dir.exists(expected_path))
dir_subdir_date <- create_dir(dir = fs::path(dir, "subdir"), add_date = TRUE)
expect_true(dir.exists(expected_path))
expect_true(endsWith(
  dir_subdir_date,
  suffix = fs::path(tempdir_basename, "testcreatedir", "temp_subdirT_dateT", "subdir",
                     format(Sys.time(), format = "%Y_%m_%d"))
))

# Checks on input to 'dir'
for(dir in list(3, "", character(0), NULL, c("temp_p1", "temp_p2"))) {
  expect_error(
    create_dir(dir = dir),
    pattern = "is_character(path) is not TRUE", fixed = TRUE)
}

for(dir in list(fs::path(my_tempdir, "temp", "."),
                fs::path(my_tempdir, "temp."),
                fs::path(my_tempdir, "temp.", "subtemp"))) {
  expect_error(
    create_dir(dir = dir),
    pattern = "'dir' should not end with ' ' or '.'", fixed = TRUE)
}

# Notes:
# - Need paste0() because fs::path() removes trailing slashes.
# - Need to use '\\\\': '\\' would test for '\'.
expect_warning(
  create_dir(dir = paste0(fs::path(my_tempdir, "temp"), "//")),
  pattern = "Repeated '/' or '\\\\'", fixed = TRUE)

expect_warning(
  create_dir(dir = paste0(fs::path(my_tempdir, "temp"), "//subtemp")),
  pattern = "Repeated '/' or '\\\\'", fixed = TRUE)

expect_warning(
  create_dir(dir = paste0(fs::path(my_tempdir), "\\\\")),
  pattern = "Repeated '/' or '\\\\'", fixed = TRUE)

expect_warning(
  create_dir(dir = paste0(fs::path(my_tempdir), "\\\\subtemp")),
  pattern = "Repeated '/' or '\\\\'", fixed = TRUE)

expect_warning(
  create_dir(dir = paste0(my_tempdir, "\\\\")),
  pattern = "Repeated '/' or '\\\\'", fixed = TRUE)

expect_warning(
  create_dir(dir = paste0(my_tempdir, "\\\\subtemp")),
  pattern = "Repeated '/' or '\\\\'", fixed = TRUE)

# NB. 'dir' equal to '.' or '..' can be used to denote the current working
# directory and its parent directory, respectively. By default, this will add
# the folder with the current date to those directories. This is not tested
# here to prevent writing in the working directory.
for(dir in list(fs::path(my_tempdir, " "),
                fs::path(my_tempdir, "temp "),
                fs::path(my_tempdir, "temp ", "subtemp"),
                fs::path(my_tempdir, "."),
                fs::path(my_tempdir, ".."),
                fs::path(my_tempdir, "temp."),
                fs::path(my_tempdir, "temp.", "subtemp"))) {
  expect_error(
    create_dir(dir = dir),
    pattern = "'dir' should not end with ' ' or '.'", fixed = TRUE)
}

# Working directory is returned if 'dir' points to a file instead of a directory
expect_warning(
  expect_true(endsWith(
    create_dir(dir = my_tempfile, add_date = FALSE),
    suffix = basename(getwd())
    )),
  pattern = "Attempt to create directory failed", strict = TRUE, fixed = TRUE)

# Checks on input to 'add_date'
for(add_date in list(1, NA)) {
  expect_error(
    create_dir(dir = my_tempdir, add_date = add_date),
    pattern = "is_logical(add_date) is not TRUE", fixed = TRUE)
}


#### Delete the created temporary files ####
unlink(my_tempdir, recursive = TRUE)


#### Remove objects used in tests ####
rm(add_date, dir, dir_no_date, dir_no_date_v2, dir_date, dir_subdir_date,
   expected_path, my_tempdir, my_tempfile, tempdir_basename)
