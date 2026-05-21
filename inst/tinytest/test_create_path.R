tinytest::report_side_effects()

#### Tests ####
##### Preparations #####
# Create temporary directory and temporary file to use in tests.
my_tempdir <- normalizePath(path = file.path(tempdir(), "testcreatepath"),
                            winslash = "/", mustWork = FALSE)
tempdir_pattern <- paste0(basename(tempdir()), ".+$")
tempdir_basename <- basename(tempdir())
current_date_Ymd <- format(Sys.time(), format = "%Y_%m_%d")
current_date_dmY <- format(Sys.time(), format = "%d_%m_%Y")

dir.create(path = my_tempdir, showWarnings = FALSE, recursive = TRUE)
my_tempfile <- normalizePath(path = file.path(my_tempdir, "test_df.csv"),
                             winslash = "/", mustWork = FALSE)
# Write csv-file, modified from example in help(write.table)
write.table(x = data.frame(a = "a", b = pi), file = my_tempfile)

##### Correct combinations #####
# Test combinations of stamp formats (absent, default or alternative format)
# with presence or absence of a date directory and a subdirectory in 'dir'.
expect_true(endsWith(
  create_path(filename = "abc.txt", format_stamp = "",
                  dir = my_tempdir, add_date = TRUE),
  suffix = file.path(tempdir_basename, "testcreatepath", current_date_Ymd,
                     "abc.txt")
))

expect_true(endsWith(
  create_path(filename = "abc.txt", format_stamp = "%d_%m_%Y",
                  dir = my_tempdir, add_date = TRUE),
  suffix = file.path(tempdir_basename, "testcreatepath", current_date_Ymd,
                     paste0(current_date_dmY, "_abc.txt"))
))

expect_true(endsWith(
  create_path(filename = "def.html", format_stamp = "",
                  dir = my_tempdir, add_date = FALSE),
  suffix = file.path(tempdir_basename, "testcreatepath", "def.html")
))

expect_true(endsWith(
  create_path(filename = "def.html", format_stamp = "%d_%m_%Y",
              dir = my_tempdir, add_date = FALSE),
  suffix = file.path(tempdir_basename, "testcreatepath", paste0(current_date_dmY, "_def.html"))
))

expect_true(endsWith(
  create_path(filename = "abc.txt", format_stamp = "",
              dir = file.path(my_tempdir, "subdir"), add_date = TRUE),
  suffix = file.path(tempdir_basename, "testcreatepath", "subdir", current_date_Ymd, "abc.txt")
))

expect_true(endsWith(
  create_path(filename = "abc.txt", format_stamp = "%d_%m_%Y",
              dir = file.path(my_tempdir, "subdir"), add_date = TRUE),
  suffix = file.path(tempdir_basename, "testcreatepath", "subdir",
                     current_date_Ymd, paste0(current_date_dmY, "_abc.txt"))
))

expect_true(endsWith(
  create_path(filename = "def.html", format_stamp = "",
              dir = file.path(my_tempdir, "subdir"), add_date = FALSE),
  suffix = file.path(tempdir_basename, "testcreatepath", "subdir", "def.html")
))

expect_true(endsWith(
  create_path(filename = "def.html", format_stamp = "%d_%m_%Y",
              dir = file.path(my_tempdir, "subdir"), add_date = FALSE),
  suffix = file.path(tempdir_basename, "testcreatepath", "subdir",
                     paste0(current_date_dmY, "_def.html"))
))

##### filename #####
for(filenm in c("a.txt", "e3f.txt", "g_g.txt")) {
  # expect_silent(
    expect_true(endsWith(
      create_path(filename = filenm, format_stamp = "",
                  dir = my_tempdir, add_date = FALSE),
      suffix = file.path(tempdir_basename, "testcreatepath", filenm)
    ))
  # )
}

expect_error(
  create_path(filename = "ff..txt", format_stamp = "",
              dir = my_tempdir, add_date = FALSE),
  pattern = "'filename' should not end with ' ' or '.'", fixed = TRUE)

##### format_stamp #####
# Characters in 'format_stamp' not part of a conversion specification in
# strptime are interpreted literally.
# expect_silent(
  expect_true(endsWith(
    create_path(filename = "abc.txt", format_stamp = "%d_%m_%Ydef",
                dir = my_tempdir, add_date = TRUE),
    suffix = file.path(tempdir_basename, "testcreatepath", current_date_Ymd,
                       paste0(current_date_dmY, "def_abc.txt"))
  ))
# )

# Non-alphanumeric characters other than underscores in the result of
# 'format_stamp' are replaced by underscores.
expect_warning(
  expect_true(endsWith(
    create_path(filename = "abc.txt", format_stamp = "%d#%m_%Ydef",
                dir = my_tempdir, add_date = TRUE),
    suffix = file.path(tempdir_basename, "testcreatepath", current_date_Ymd,
                       paste0(format(Sys.time(), format = "%d.%m_%Y"), "def_abc.txt"))
  )),
  pattern = "Replaced non-alphanumeric characters other than underscores",
  strict = TRUE, fixed = TRUE
)

# expect_silent(
  expect_true(
    grepl(pattern = paste0("testcreatepath.[[:digit:]]{2}_[[:digit:]]{2}",
                           "_[[:digit:]]{2}\\.[[:digit:]]{3}_test1c.txt$"),
          x = create_path(filename = "test1c.txt", format_stamp = "%H_%M_%OS3",
                          dir = my_tempdir, add_date = FALSE))
  )
# )

# expect_silent(
  expect_true(
    grepl(pattern = paste0("testcreatepath.[[:digit:]]{2}_[[:digit:]]{2}",
                           "_[[:digit:]]{2}\\.[[:digit:]]{5}_test1c.txt$"),
          x = create_path(filename = "test1c.txt", format_stamp = "%H_%M_%OS5",
                          dir = my_tempdir, add_date = FALSE))
  )
# )

##### dir #####
# 'directories' that are only working directory followed by a file extension
# are accepted!
# expect_silent(
#   expect_identical(
#     create_path(filename = "abc.txt", format_stamp = "",
#                 dir = file.path(my_tempdir, ".txt"), add_date = FALSE),
#     normalizePath(file.path(my_tempdir, ".txt", "abc.txt"),
#                   winslash = "/", mustWork = FALSE)
#   )
# )

##### Warnings #####
# 'directories' that actually are names of existing files lead to the working
# directory being used instead, with warnings that the file already exists and
# that the working directory is used because creation of the directory failed.
# expect_warning(
#   expect_true(
#     grepl(
#       pattern = file.path(basename(getwd()), "abc.txt", fsep = "/"),
#       x = create_path(filename = "abc.txt", format_stamp = "",
#                       dir = my_tempfile, add_date = FALSE),
#       ignore.case = FALSE, fixed = TRUE)),
#   pattern = paste0(basename(my_tempfile), "' already exists"),
#   strict = TRUE, fixed = TRUE
# )
#
# expect_warning(
#     create_path(filename = "abc.txt", format_stamp = "",
#                 dir = my_tempfile, add_date = FALSE),
#   pattern = "Attempt to create directory", strict = TRUE, fixed = TRUE
# )
#
# # A warning is issued if the file indicated by the returned path already exists.
# expect_warning(
#   expect_identical(
#     create_path(filename = basename(my_tempfile), format_stamp = "",
#                 dir = my_tempdir, add_date = FALSE),
#     my_tempfile),
#   pattern = "File already exists:", strict = TRUE, fixed = TRUE)
#
# filenm_in <- c("c#c.txt", "d d.txt")
# filenm_out <- c("c.c.txt", "d.d.txt")
# for(ind_filenm in seq_along(filenm_in)) {
#   expect_warning(
#     expect_identical(
#       create_path(filename = filenm_in[ind_filenm], format_stamp = "",
#                   dir = my_tempdir, add_date = FALSE),
#       normalizePath(file.path(my_tempdir, filenm_out[ind_filenm]),
#                     winslash = "/", mustWork = FALSE)
#     ),
#     pattern = paste0("Replaced non-alphanumeric characters other than",
#                      " underscores in filename\n'", filenm_in[ind_filenm],
#                      "' with dots: ", filenm_out[ind_filenm]),
#     strict = TRUE, fixed = TRUE)
# }

##### Check 'filename' #####
expect_error(create_path(dir = my_tempdir),
             pattern = "argument \"filename\" is missing, with no default",
             fixed = TRUE)
expect_error(create_path(filename = 123, dir = my_tempdir),
             pattern = "checkinput::is_character(filename) is not TRUE",
             fixed = TRUE)
expect_error(create_path(filename = c("abc.txt", "def.txt"), dir = my_tempdir),
             pattern = "checkinput::is_character(filename) is not TRUE",
             fixed = TRUE)
expect_error(create_path(filename = "", dir = my_tempdir),
             pattern = "checkinput::is_character(filename) is not TRUE",
             fixed = TRUE)

for(filenm_in in c("abcd", "abc.", "ab.c#", ".", ".txt", ".html")) {
  expect_error(create_path(filename = filenm_in, dir = my_tempdir),
               pattern = "No path created: Empty filename or missing extension",
               fixed = TRUE)
}

filenm_in <- c("a/a.txt", "b\\b.txt")
for(ind_filenm in seq_along(filenm_in)) {
  expect_error(
    create_path(filename = filenm_in[ind_filenm], format_stamp = "",
                dir = my_tempdir, add_date = FALSE),
    pattern = paste0("'filename' contains slashes or backslashes"),
    fixed = TRUE)
}

##### Check 'format_stamp' #####
expect_error(
  create_path(filename = "abc.txt", format_stamp = character(0),
              dir = my_tempdir, add_date = TRUE),
  pattern = paste0("checkinput::is_character(format_stamp, allow_empty = TRUE)",
                   " is not TRUE"), fixed = TRUE)

expect_error(
  create_path(filename = "abc.txt", format_stamp = NA_character_,
              dir = my_tempdir, add_date = TRUE),
  pattern = paste0("checkinput::is_character(format_stamp, allow_empty = TRUE)",
                   " is not TRUE"), fixed = TRUE)

expect_error(
  create_path(filename = "abc.txt", format_stamp = c("", "%d_%m_%Y"),
              dir = my_tempdir, add_date = TRUE),
  pattern = paste0("checkinput::is_character(format_stamp, allow_empty = TRUE)",
                   " is not TRUE"), fixed = TRUE)

##### Check 'dir' #####
for(dir in list(3, "", character(0), NULL, c("temp_p1", "temp_p2"))) {
  expect_error(
    create_path(filename = "abc.txt", dir = dir),
    pattern = "checkinput::is_character(dir) is not TRUE", fixed = TRUE)
}

for(dir in list(paste0(my_tempdir, "./"),
                paste0(my_tempdir, "temp_p1/"),
                paste0(my_tempdir, ".\\"),
                paste0(my_tempdir, "temp_p1\\"))) {
  expect_warning(
    create_path(filename = "abc.txt", dir = dir),
    pattern = "Repeated '/' or '\\'", fixed = TRUE)
}

for(dir in list(paste0(my_tempdir, ".."),
                paste0(my_tempdir, "temp_p1."),
                paste0(my_tempdir, "."),
                file.path(my_tempdir, "."),
                paste0(my_tempdir, ". "),
                paste0(my_tempdir, "temp_p1 "))) {
  expect_error(
    create_path(filename = "abc.txt", dir = dir),
    pattern = "should not end with ' ' or '.'", fixed = TRUE)
}

##### Check 'add_date' #####
for(add_date in list(3, NA)) {
  expect_error(
    create_path(filename = "abc.txt", dir = my_tempdir, add_date = add_date),
    pattern = "checkinput::is_logical(add_date) is not TRUE", fixed = TRUE)
}


#### Cleaning up ####
##### Delete the created temporary files #####
unlink(x = my_tempdir, recursive = TRUE)


##### Remove objects used in tests #####
rm(add_date, dir, # filenm,
   filenm_in, # filenm_out,
   ind_filenm, my_tempdir,
   my_tempfile)
