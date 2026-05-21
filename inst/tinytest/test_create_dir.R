tinytest::report_side_effects()


#### Test the examples ####
my_tempdir <- normalizePath(path = file.path(tempdir(), "testcreatedir"),
                            winslash = "/", mustWork = FALSE)
tempdir_basename <- basename(tempdir())

res_dir_one <- create_dir(dir = file.path(my_tempdir, "dir_one"),
                          add_date = FALSE)
expect_true(dir.exists(res_dir_one))

res_dir_one_v2 <- create_dir(dir = file.path(my_tempdir, "dir_one"),
                             add_date = FALSE)
expect_true(endsWith(
  res_dir_one,
  suffix = file.path(tempdir_basename, "testcreatedir", "dir_one")
))
expect_true(endsWith(
  res_dir_one_v2,
  suffix = file.path(tempdir_basename, "testcreatedir", "dir_one")
))

# On case-insensitive file systems such as Windows and macOS, the directory
# created below is the same as 'res_dir_one'. On case-sensitive file systems
# such as Ubuntu, it differs in case from 'res_dir_one'.
# To do:
# - Issues a spurious warning on MacOS because there the part before the output
#   of 'tempdir()' contains repeated slashes. Can re-wrap in 'expect_silent()'
#   if only the part starting with the output of 'tempdir()' is tested to
#   prevent the spurious warning.
create_dir(dir = file.path(my_tempdir, "dir_ONE"), add_date = FALSE)

res_dir_two <- create_dir(dir = file.path(my_tempdir, "dir_two"),
                          add_date = TRUE)
expect_true(dir.exists(res_dir_two))

# Cleaning up
unlink(dirname(res_dir_one), recursive = TRUE)
rm(my_tempdir, res_dir_one, res_dir_one_v2, res_dir_two, tempdir_basename)


#### Tests ####
my_tempdir <- normalizePath(path = file.path(tempdir(), "testcreatedir"),
                            winslash = "/", mustWork = FALSE)
dir.create(my_tempdir)
tempdir_basename <- basename(tempdir())
my_tempfile <- file.path(my_tempdir, "test_df.csv")
# Write csv-file, modified from example in help(write.table)
write.table(x = data.frame(a = "a", b = pi), file = my_tempfile)

# without date directory
dir <- file.path(my_tempdir, "temp_subdirF_dateF")
expected_path <- dir

expect_false(dir.exists(expected_path))
dir_no_date <- create_dir(dir = dir, add_date = FALSE)
expect_true(dir.exists(expected_path))
expect_true(endsWith(
  dir_no_date,
  suffix = file.path(tempdir_basename, "testcreatedir", "temp_subdirF_dateF")
))

# without date directory, directory already exists
dir_no_date_v2 <- create_dir(dir = dir, add_date = FALSE)
expect_true(endsWith(
  dir_no_date_v2,
  suffix = file.path(tempdir_basename, "testcreatedir", "temp_subdirF_dateF")
))

# with date directory
dir <- file.path(my_tempdir, "temp_subdirF_dateT")
expected_path <- file.path(dir, format(Sys.time(), format = "%Y_%m_%d"))

expect_false(dir.exists(expected_path))
dir_date <- create_dir(dir = dir, add_date = TRUE)
expect_true(dir.exists(expected_path))
expect_true(endsWith(
  dir_date,
  suffix = file.path(tempdir_basename, "testcreatedir", "temp_subdirF_dateT",
                     format(Sys.time(), format = "%Y_%m_%d"))
))

# with subdirectories, with date directory
dir <- file.path(my_tempdir, "temp_subdirT_dateT")
expected_path <- file.path(dir, "subdir", format(Sys.time(), format = "%Y_%m_%d"))

expect_false(dir.exists(expected_path))
dir_subdir_date <- create_dir(dir = file.path(dir, "subdir"), add_date = TRUE)
expect_true(dir.exists(expected_path))
expect_true(endsWith(
  dir_subdir_date,
  suffix = file.path(tempdir_basename, "testcreatedir", "temp_subdirT_dateT", "subdir",
                     format(Sys.time(), format = "%Y_%m_%d"))
))

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
  expect_warning(
    create_dir(dir = dir),
    # Need to use '\\': '\' would test for ''.
    pattern = "Repeated '/' or '\\'", fixed = TRUE)
}

# NB. 'dir' equal to '.' or '..' can be used to denote the current working
# directory and its parent directory, respectively. By default, this will add
# the folder with the current date to those directories. This is not tested
# here to prevent writing in the working directory.
for(dir in list(file.path(my_tempdir, " "),
                file.path(my_tempdir, "temp "),
                file.path(my_tempdir, "temp ", "subtemp"),
                file.path(my_tempdir, "."),
                file.path(my_tempdir, ".."),
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
rm(add_date, dir, dir_no_date, dir_no_date_v2, dir_date, dir_subdir_date,
   expected_path, my_tempdir, my_tempfile, tempdir_basename)
