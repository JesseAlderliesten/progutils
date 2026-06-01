tinytest::report_side_effects()


#### Tests ####
my_tempdir <- normalizePath(path = tempdir(), winslash = "/", mustWork = FALSE)

# Possible to create a temporary subdirectory
expect_false(dir.exists(fs::path(my_tempdir, "createtempdir")))
res_subdir <- create_tempdir(subdir = "createtempdir")
expect_true(dir.exists(res_subdir))

# Error if temporary subdirectory already exists
expect_error(create_tempdir(subdir = "createtempdir"),
             pattern = paste0("Temporary directory already exists: change",
                              " 'subdir' ('createtempdir')"), fixed = TRUE)

# Temporary subdirectory is writeable
my_tempfile <- fs::path(res_subdir, "test_df.csv")
expect_false(file.exists(my_tempfile))
# Write csv-file, modified from example in help(write.table)
write.table(x = data.frame(a = "a", b = pi), file = my_tempfile)
expect_true(file.exists(my_tempfile))

# Target points to a file instead of a directory
expect_warning(
  expect_error(
    create_tempdir(subdir = fs::path(basename(dirname(my_tempfile)),
                                     basename(my_tempfile))),
    pattern = "create a subdirectory in the temporary directory failed",
    fixed = TRUE),
  pattern = "already exists", fixed = TRUE, strict = TRUE)

# Also recognise that a directory already exists if it has subdirectories
expect_false(dir.exists(fs::path(my_tempdir, "subdir", "abc")))
res_subdir_recursive <- create_tempdir(subdir = fs::path("subdir", "abc"))
expect_true(dir.exists(res_subdir_recursive))

# Checks on input to 'subdir'
for(subdir in list(3, "", character(0), NULL, c("temp_p1", "temp_p2"))) {
  expect_error(
    create_tempdir(subdir = subdir),
    pattern = "is_character(subdir) is not TRUE", fixed = TRUE)
}

# Trailing '\\' is changed to '/' but trailing '/' is removed
subdir_in <- c("tem\\p_p1", "temp_p2\\", "tem/p_p3", "temp_p4/")
subdir_out <- c("tem/p_p1", "temp_p2", "tem/p_p3", "temp_p4")
for(ind_subdir in seq_along(subdir_in)) {
  expect_true(endsWith(
    create_tempdir(subdir = subdir_in[ind_subdir]),
    # Need paste0() because fs::path() removes trailing slashes
    paste0(basename(my_tempdir), "/", subdir_out[ind_subdir])
  ))
}

expect_silent(
  expect_true(endsWith(
    create_tempdir(subdir = "\\temp_p5"),
    suffix = fs::path(basename(my_tempdir), "temp_p5")
  ))
)

expect_silent(
  expect_true(endsWith(
    create_tempdir(subdir = "/temp_p6"),
    suffix = fs::path(basename(my_tempdir), "temp_p6")
  ))
)

expect_error(
  create_tempdir(subdir = "."),
  pattern = "would write to 'tempdir()'", fixed = TRUE)

expect_error(
  create_tempdir(subdir = ".."),
  pattern = "would write above 'tempdir()'", fixed = TRUE)

for(subdir in list("temp_p7.", "temp_p8 ")) {
  expect_warning(
    expect_error(
      create_tempdir(subdir = subdir),
      pattern = "is_path(subdir) is not TRUE", fixed = TRUE),
    pattern = "should not end with ' ' or '.'", strict = TRUE, fixed = TRUE)
}


#### Delete the created temporary files ####
unlink(c(res_subdir, dirname(res_subdir_recursive),
         fs::path(my_tempdir, subdir_out), fs::path(my_tempdir, "temp_p5"),
         fs::path(my_tempdir, "temp_p6"), fs::path(my_tempdir, "tem")),
       recursive = TRUE)


#### Remove objects used in tests ####
rm(ind_subdir, my_tempdir, my_tempfile, res_subdir, res_subdir_recursive,
   subdir, subdir_in, subdir_out)
