tinytest::report_side_effects()


#### Test the examples ####
my_tempdir <- create_tempdir(pattern = "examplecreatedir")
basename_my_tempdir <- basename(my_tempdir)

res_dir_one <- create_dir(dir = fs::path(my_tempdir, "dir_one"),
                          add_date = FALSE)
res_dir_one_v2 <- create_dir(dir = fs::path(my_tempdir, "dir_one"),
                             add_date = FALSE)
# On case-insensitive file systems such as Windows and macOS, the directory
# 'res_dir_ONE' is the same as 'res_dir_one'. On case-sensitive file systems
# such as Ubuntu, it differs in case from 'res_dir_one'.
res_dir_ONE <- create_dir(dir = fs::path(my_tempdir, "dir_ONE"),
                          add_date = FALSE)
res_dir_date <- create_dir(dir = fs::path(my_tempdir, "dir_date"),
                           add_date = TRUE)

# Directories exist
expect_true(
  all(fs::dir_exists(c(res_dir_one, res_dir_one_v2, res_dir_ONE, res_dir_date)))
)

# Directories are inside 'my_tempdir'
expect_true(
  all(basename(dirname(c(res_dir_one, res_dir_one_v2, res_dir_ONE))) ==
        basename_my_tempdir)
)

# Directory is named according to 'dir'
expect_true(
  endsWith(res_dir_one, suffix = fs::path(basename_my_tempdir, "dir_one"))
)
expect_true(
  endsWith(res_dir_one_v2, suffix = fs::path(basename_my_tempdir, "dir_one"))
)
expect_true(
  endsWith(res_dir_date,
           suffix = fs::path(basename_my_tempdir, "dir_date",
                             format(Sys.time(), format = "%Y_%m_%d"))
  )
)

# Cleaning up
unlink(my_tempdir, recursive = TRUE)
rm(my_tempdir, res_dir_one, res_dir_one_v2, res_dir_ONE, res_dir_date,
   basename_my_tempdir)


#### Tests ####
my_tempdir <- create_tempdir(pattern = "testcreatedir")
basename_my_tempdir <- basename(my_tempdir)
pattern_temp <- fs::path(basename_my_tempdir, "temp")

my_tempfile <- fs::path(my_tempdir, "test_df.csv")
# Write csv-file, modified from example in help(write.table)
write.table(x = data.frame(a = "a", b = pi), file = my_tempfile)


# without date directory
dir <- fs::path(my_tempdir, "temp_subdirF_dateF")
expected_path <- dir

expect_false(fs::dir_exists(expected_path))
dir_no_date <- create_dir(dir = dir, add_date = FALSE)
expect_true(fs::dir_exists(expected_path))
expect_true(endsWith(
  dir_no_date,
  suffix = fs::path(basename_my_tempdir, "temp_subdirF_dateF")
))

# without date directory, directory already exists
dir_no_date_v2 <- create_dir(dir = dir, add_date = FALSE)
expect_true(endsWith(
  dir_no_date_v2,
  suffix = fs::path(basename_my_tempdir, "temp_subdirF_dateF")
))

# with date directory
dir <- fs::path(my_tempdir, "temp_subdirF_dateT")
expected_path <- fs::path(dir, format(Sys.time(), format = "%Y_%m_%d"))

expect_false(fs::dir_exists(expected_path))
dir_date <- create_dir(dir = dir, add_date = TRUE)
expect_true(fs::dir_exists(expected_path))
expect_true(endsWith(
  dir_date,
  suffix = fs::path(basename_my_tempdir, "temp_subdirF_dateT",
                    format(Sys.time(), format = "%Y_%m_%d"))
))

# with subdirectories, with date directory
dir <- fs::path(my_tempdir, "temp_subdirT_dateT")
expected_path <- fs::path(dir, "subdir", format(Sys.time(), format = "%Y_%m_%d"))

expect_false(fs::dir_exists(expected_path))
dir_subdir_date <- create_dir(dir = fs::path(dir, "subdir"), add_date = TRUE)
expect_true(fs::dir_exists(expected_path))
expect_true(endsWith(
  dir_subdir_date,
  suffix = fs::path(basename_my_tempdir, "temp_subdirT_dateT", "subdir",
                    format(Sys.time(), format = "%Y_%m_%d"))
))

# Checks on input to 'dir'
for(dir in list(3, "", character(0), NULL, c("temp_p1", "temp_p2"))) {
  expect_error(
    create_dir(dir = dir),
    pattern = "is_character(dir) is not TRUE", fixed = TRUE)
}

for(dir in list(fs::path(my_tempdir, "temp", "."),
                fs::path(my_tempdir, "temp."),
                fs::path(my_tempdir, "temp.", "subtemp"))) {
  expect_warning(
    expect_error(
      create_dir(dir = dir),
      pattern = "is_path(dir) is not TRUE", fixed = TRUE),
    pattern = "Components of 'dir' should not end with ' ' or '.'",
    fixed = TRUE, strict = TRUE)
}

# Notes:
# - Need paste0() because fs::path() removes trailing slashes.
expect_silent(expect_true(
  grepl(pattern = pattern_temp,
        x = create_dir(dir = paste0(fs::path(my_tempdir, "temp_p3"), "//")))
))

expect_silent(expect_true(
  grepl(pattern = fs::path(pattern_temp, "subtemp_p4"),
        x = create_dir(dir = paste0(fs::path(my_tempdir, "temp"), "//subtemp_p4")))
))

expect_silent(expect_true(
  grepl(pattern = basename_my_tempdir,
        x = create_dir(dir = paste0(fs::path(my_tempdir), "\\\\")))
))

expect_silent(expect_true(
  grepl(pattern = pattern_temp,
        x = create_dir(dir = paste0(fs::path(my_tempdir), "\\\\temp")))
))

expect_silent(expect_true(
  grepl(pattern = basename_my_tempdir,
        x = create_dir(dir = paste0(my_tempdir, "\\\\")))
))

expect_silent(expect_true(
  grepl(pattern = pattern_temp,
        x = create_dir(dir = paste0(my_tempdir, "\\\\temp")))
))

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
  expect_warning(
    expect_error(
      create_dir(dir = dir),
      pattern = "checkinput::is_path(dir) is not TRUE", fixed = TRUE),
    pattern = "Components of 'dir' should not end with ' ' or '.'",
    fixed = TRUE, strict = TRUE)
}

# Throw an error if 'dir' points to a file instead of a directory (versions up
# to 0.4.0 returned the working directory but that is unsafe).
expect_error(
  create_dir(dir = my_tempfile, add_date = FALSE),
  pattern = "Failed to make directory", fixed = TRUE)

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
   expected_path, my_tempdir, my_tempfile, pattern_temp,
   basename_my_tempdir)
