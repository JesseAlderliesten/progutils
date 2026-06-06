tinytest::report_side_effects()

#### Tests ####
##### Preparations #####
# Create temporary directory and temporary file to use in tests.
my_tempdir <- fs::path_abs(path = fs::path(tempdir(), "testcreatepath"))
tempdir_pattern <- paste0(basename(tempdir()), ".+$")
tempdir_basename <- basename(tempdir())
current_date_Ymd <- format(Sys.time(), format = "%Y_%m_%d")
current_date_dmY <- format(Sys.time(), format = "%d_%m_%Y")

fs::dir_create(path = my_tempdir, showWarnings = FALSE)
my_tempfile <- fs::path_abs(path = fs::path(my_tempdir, "test_df.csv"))
# Write csv-file, modified from example in help(write.table)
write.table(x = data.frame(a = "a", b = pi), file = my_tempfile)

##### Correct combinations #####
# Test combinations of stamp formats (absent, default or alternative format)
# with presence or absence of a date directory and a subdirectory in 'dir'.
expect_true(endsWith(
  create_file_path(filename = "abc.txt", format_stamp = "",
                   dir = my_tempdir, add_date = TRUE),
  suffix = fs::path(tempdir_basename, "testcreatepath", current_date_Ymd,
                    "abc.txt")
))

expect_true(endsWith(
  create_file_path(filename = "abc.txt", format_stamp = "%d_%m_%Y",
                   dir = my_tempdir, add_date = TRUE),
  suffix = fs::path(tempdir_basename, "testcreatepath", current_date_Ymd,
                    paste0(current_date_dmY, "_abc.txt"))
))

expect_true(endsWith(
  create_file_path(filename = "def.html", format_stamp = "",
                   dir = my_tempdir, add_date = FALSE),
  suffix = fs::path(tempdir_basename, "testcreatepath", "def.html")
))

expect_true(endsWith(
  create_file_path(filename = "def.html", format_stamp = "%d_%m_%Y",
                   dir = my_tempdir, add_date = FALSE),
  suffix = fs::path(tempdir_basename, "testcreatepath", paste0(current_date_dmY, "_def.html"))
))

expect_true(endsWith(
  create_file_path(filename = "abc.txt", format_stamp = "",
                   dir = fs::path(my_tempdir, "subdir"), add_date = TRUE),
  suffix = fs::path(tempdir_basename, "testcreatepath", "subdir", current_date_Ymd, "abc.txt")
))

expect_true(endsWith(
  create_file_path(filename = "abc.txt", format_stamp = "%d_%m_%Y",
                   dir = fs::path(my_tempdir, "subdir"), add_date = TRUE),
  suffix = fs::path(tempdir_basename, "testcreatepath", "subdir",
                    current_date_Ymd, paste0(current_date_dmY, "_abc.txt"))
))

expect_true(endsWith(
  create_file_path(filename = "def.html", format_stamp = "",
                   dir = fs::path(my_tempdir, "subdir"), add_date = FALSE),
  suffix = fs::path(tempdir_basename, "testcreatepath", "subdir", "def.html")
))

expect_true(endsWith(
  create_file_path(filename = "def.html", format_stamp = "%d_%m_%Y",
                   dir = fs::path(my_tempdir, "subdir"), add_date = FALSE),
  suffix = fs::path(tempdir_basename, "testcreatepath", "subdir",
                    paste0(current_date_dmY, "_def.html"))
))

##### filename #####
for(filenm in c("a.txt", "c#c.txt", "d d.txt", "e3f.txt", "g_g.g.txt", "ab.c#")) {
  expect_true(endsWith(
    create_file_path(filename = filenm, format_stamp = "",
                     dir = my_tempdir, add_date = FALSE),
    suffix = fs::path(tempdir_basename, "testcreatepath", filenm)
  ))
}

for(filenm_in in c("abcd", ".", ".txt", ".html")) {
  expect_error(create_file_path(filename = filenm_in, dir = my_tempdir,
                                format_stamp = ""),
               pattern = "Empty filename or missing extension",
               fixed = TRUE)
}

expect_warning(
  expect_error(
    create_file_path(filename = "abc.", dir = my_tempdir, format_stamp = ""),
    pattern = "is_path(filename, require_sep = FALSE) is not TRUE", fixed = TRUE),
  pattern = "Components of 'filename' should not end with ' ' or '.'",
  strict = TRUE, fixed = TRUE)

filenm_in <- c("a/a.txt", "b\\b.txt")
for(ind_filenm in seq_along(filenm_in)) {
  expect_error(
    create_file_path(filename = filenm_in[ind_filenm], format_stamp = "",
                     dir = my_tempdir, add_date = FALSE),
    pattern = paste0("should not contain '/' or '\\'"),
    fixed = TRUE)
}

expect_warning(
  expect_error(
    create_file_path(filename = "ff .txt", format_stamp = "",
                     dir = my_tempdir, add_date = FALSE),
    pattern = "is_path(filename, require_sep = FALSE) is not TRUE", fixed = TRUE),
  pattern = "'filename' should not end with ' ' or '.'", strict = TRUE, fixed = TRUE)

# No warning message specified: on Windows with R 4.6.0 the warning message is
# '"'filename' should not end with ' ' or '.'"', but on Windows with R 4.1.0 the
# error message is "Empty filename or missing extension"
expect_warning(
  expect_error(
    create_file_path(filename = "ff..txt", format_stamp = "", dir = my_tempdir,
                     add_date = FALSE))
)

##### format_stamp #####
# Characters in 'format_stamp' not part of a conversion specification in
# strptime are interpreted literally.
expect_true(endsWith(
  create_file_path(filename = "abc.txt", format_stamp = "%d_%m_%Ydef",
                   dir = my_tempdir, add_date = TRUE),
  suffix = fs::path(tempdir_basename, "testcreatepath", current_date_Ymd,
                    paste0(current_date_dmY, "def_abc.txt"))
))

# Non-alphanumeric characters are *not* replaced (they used to be replaced in
# earlier versions.
expect_true(endsWith(
  create_file_path(filename = "abc.txt", format_stamp = "%d#.%m_%Ydef",
                   dir = my_tempdir, add_date = TRUE),
  suffix = fs::path(tempdir_basename, "testcreatepath", current_date_Ymd,
                    paste0(format(Sys.time(), format = "%d#.%m_%Y"), "def_abc.txt"))
))

expect_true(
  grepl(pattern = paste0("testcreatepath.[[:digit:]]{2}_[[:digit:]]{2}",
                         "_[[:digit:]]{2}\\.[[:digit:]]{3}_test1c.txt$"),
        x = create_file_path(filename = "test1c.txt", format_stamp = "%H_%M_%OS3",
                             dir = my_tempdir, add_date = FALSE))
)

expect_true(
  grepl(pattern = paste0("testcreatepath.[[:digit:]]{2}_[[:digit:]]{2}",
                         "_[[:digit:]]{2}\\.[[:digit:]]{5}_test1c.txt$"),
        x = create_file_path(filename = "test1c.txt", format_stamp = "%H_%M_%OS5",
                             dir = my_tempdir, add_date = FALSE))
)

expect_error(
  create_file_path(filename = "abc.txt", format_stamp = character(0),
                   dir = my_tempdir, add_date = TRUE),
  pattern = paste0("checkinput::is_character(format_stamp, allow_empty = TRUE)",
                   " is not TRUE"), fixed = TRUE)

expect_error(
  create_file_path(filename = "abc.txt", format_stamp = NA_character_,
                   dir = my_tempdir, add_date = TRUE),
  pattern = paste0("checkinput::is_character(format_stamp, allow_empty = TRUE)",
                   " is not TRUE"), fixed = TRUE)

expect_error(
  create_file_path(filename = "abc.txt", format_stamp = c("", "%d_%m_%Y"),
                   dir = my_tempdir, add_date = TRUE),
  pattern = paste0("checkinput::is_character(format_stamp, allow_empty = TRUE)",
                   " is not TRUE"), fixed = TRUE)

##### dir #####
# 'directories' might contain a file extension
expect_true(endsWith(
  create_file_path(filename = "abc.txt", format_stamp = "",
                   dir = fs::path(my_tempdir, ".txt"), add_date = FALSE),
  fs::path(tempdir_basename, "testcreatepath", ".txt", "abc.txt")
))

expect_error(
  create_file_path(filename = "testfile123.csv", format_stamp = "",
                   dir = my_tempfile, add_date = FALSE),
  pattern = "Failed to make directory", fixed = TRUE
)

for(dir in list(3, "", character(0), NULL, c("temp_p1", "temp_p2"))) {
  expect_error(
    create_file_path(filename = "abc.txt", dir = dir),
    pattern = "is_character(dir) is not TRUE", fixed = TRUE)
}

for(dir in list(paste0(my_tempdir, "//"),
                paste0(my_tempdir, "//temp_p1"))) {
  expect_silent(create_file_path(filename = "abc.txt", dir = dir))
}

for(dir in list(paste0(my_tempdir, ".."),
                paste0(my_tempdir, "temp_p1."),
                paste0(my_tempdir, "."),
                fs::path(my_tempdir, "."),
                paste0(my_tempdir, ". "),
                paste0(my_tempdir, "temp_p1 "))) {
  expect_warning(
    expect_error(
      create_file_path(filename = "abc.txt", dir = dir),
      pattern = "is_path(dir) is not TRUE", fixed = TRUE),
    pattern = "should not end with ' ' or '.'", strict = TRUE, fixed = TRUE)
}

##### Warnings #####
# A warning is issued if the file indicated by the returned path already exists.
my_tempfile <- fs::path(my_tempdir, "testfile.csv")
write.table(x = data.frame(a = "a", b = pi), file = my_tempfile)
expect_warning(
  expect_true(endsWith(
    create_file_path(filename = basename(my_tempfile), format_stamp = "",
                     dir = my_tempdir, add_date = FALSE),
    suffix = fs::path(tempdir_basename, "testcreatepath", basename(my_tempfile)))),
  pattern = "File already exists:", strict = TRUE, fixed = TRUE)


#### Invalid input ####

##### filename #####
expect_error(create_file_path(dir = my_tempdir),
             pattern = "argument \"filename\" is missing, with no default",
             fixed = TRUE)
expect_error(create_file_path(filename = 123, dir = my_tempdir),
             pattern = "checkinput::is_character(filename) is not TRUE",
             fixed = TRUE)
expect_error(create_file_path(filename = c("abc.txt", "def.txt"), dir = my_tempdir),
             pattern = "checkinput::is_character(filename) is not TRUE",
             fixed = TRUE)
expect_error(create_file_path(filename = "", dir = my_tempdir),
             pattern = "checkinput::is_character(filename) is not TRUE",
             fixed = TRUE)

##### add_date #####
for(add_date in list(1, NA)) {
  expect_error(
    create_file_path(filename = "abc.txt", dir = my_tempdir, add_date = add_date),
    pattern = "checkinput::is_logical(add_date) is not TRUE", fixed = TRUE)
}


#### Cleaning up ####
##### Delete the created temporary files #####
unlink(x = my_tempdir, recursive = TRUE)


##### Remove objects used in tests #####
rm(add_date, current_date_dmY, current_date_Ymd, dir, filenm, filenm_in,
   ind_filenm, my_tempdir, my_tempfile, tempdir_basename, tempdir_pattern)
