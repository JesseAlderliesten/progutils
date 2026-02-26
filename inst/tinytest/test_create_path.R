#### Test the examples ####
# Examples are tested in section 'Correct combinations' below


#### Tests ####
##### Preparations #####
# Create temporary directory and temporary file to use in tests.
my_tempdir <- normalizePath(path = tempdir(), winslash = "/", mustWork = FALSE)
my_tempfile <- normalizePath(path = file.path(my_tempdir, "test_df.csv"),
                             winslash = "/", mustWork = FALSE)
# Write csv-file, modified from example in help(write.table)
write.table(x = data.frame(a = "a", b = pi), file = my_tempfile)

##### Correct combinations #####
# Test combinations of stamp formats (absent, default or alternative format)
# with presence or absence of a date directory and a subdirectory in 'dir'.
expect_silent(
  expect_identical(
    create_path(filename = "abc.txt", format_stamp = "",
                dir = my_tempdir, add_date = TRUE),
    file.path(my_tempdir, format(Sys.time(), format = "%Y_%m_%d"), "abc.txt")
  )
)

expect_silent(
  expect_identical(
    create_path(filename = "abc.txt", format_stamp = "%d_%m_%Y",
                dir = my_tempdir, add_date = TRUE),
    file.path(my_tempdir, format(Sys.time(), format = "%Y_%m_%d"),
              paste0(format(Sys.time(), format = "%d_%m_%Y"), "_abc.txt"))
  )
)

expect_silent(
  expect_identical(
    create_path(filename = "def.html", format_stamp = "",
                dir = my_tempdir, add_date = FALSE),
    file.path(my_tempdir, "def.html")
  )
)

expect_silent(
  expect_identical(
    create_path(filename = "def.html", format_stamp = "%d_%m_%Y",
                dir = my_tempdir, add_date = FALSE),
    file.path(my_tempdir,
              paste0(format(Sys.time(), format = "%d_%m_%Y"), "_def.html"))
  )
)

expect_silent(
  expect_identical(
    create_path(filename = "abc.txt", format_stamp = "",
                dir = file.path(my_tempdir, "subdir"), add_date = TRUE),
    file.path(my_tempdir, "subdir", format(Sys.time(), format = "%Y_%m_%d"), "abc.txt")
  )
)

expect_silent(
  expect_identical(
    create_path(filename = "abc.txt", format_stamp = "%d_%m_%Y",
                dir = file.path(my_tempdir, "subdir"), add_date = TRUE),
    file.path(my_tempdir, "subdir", format(Sys.time(), format = "%Y_%m_%d"),
              paste0(format(Sys.time(), format = "%d_%m_%Y"), "_abc.txt"))
  )
)

expect_silent(
  expect_identical(
    create_path(filename = "def.html", format_stamp = "",
                dir = file.path(my_tempdir, "subdir"), add_date = FALSE),
    file.path(my_tempdir, "subdir", "def.html")
  )
)

expect_silent(
  expect_identical(
    create_path(filename = "def.html", format_stamp = "%d_%m_%Y",
                dir = file.path(my_tempdir, "subdir"), add_date = FALSE),
    file.path(my_tempdir, "subdir",
              paste0(format(Sys.time(), format = "%d_%m_%Y"), "_def.html"))
  )
)

##### filename #####
# File names that end in a dot are handled incorrectly by
# tools::file_path_sans_ext(), so progutils::file_path_sans_ext() is used, see
# the 'Programming note' in create_path().
for(filenm in c("a.txt", "e3f.txt", "ff..txt", "g_g.txt")) {
  expect_silent(
    expect_identical(
      create_path(filename = filenm, format_stamp = "",
                  dir = my_tempdir, add_date = FALSE),
      normalizePath(file.path(my_tempdir, filenm),
                    winslash = "/", mustWork = FALSE)
    )
  )
}

##### format_stamp #####
# Characters in 'format_stamp' not part of a conversion specification in
# strptime are interpreted literally.
expect_silent(
  expect_identical(
    create_path(filename = "abc.txt", format_stamp = "%d_%m_%Ydef",
                dir = my_tempdir, add_date = TRUE),
    file.path(my_tempdir, format(Sys.time(), format = "%Y_%m_%d"),
              paste0(format(Sys.time(), format = "%d_%m_%Y"), "def_abc.txt"))
  )
)

# Non-alphanumeric characters other than underscores in the result of
# 'format_stamp' are replaced by underscores.
expect_warning(
  expect_identical(
    create_path(filename = "abc.txt", format_stamp = "%d#%m_%Ydef",
                dir = my_tempdir, add_date = TRUE),
    file.path(my_tempdir, format(Sys.time(), format = "%Y_%m_%d"),
              paste0(format(Sys.time(), format = "%d.%m_%Y"), "def_abc.txt"))
  ), pattern = "Replaced non-alphanumeric characters other than underscores",
  strict = TRUE, fixed = TRUE
)

expect_silent(
  expect_equal(
    nchar(basename(
      create_path(filename = "test1c.txt", format_stamp = "%H_%M_%OS3",
                  dir = my_tempdir, add_date = FALSE))),
    23L
  )
)

expect_silent(
  expect_equal(
    nchar(basename(
      create_path(filename = "test1c.txt", format_stamp = "%H_%M_%OS5",
                  dir = my_tempdir, add_date = FALSE))),
    25L
  )
)

##### dir #####
# "." is the working directory
expect_silent(
  expect_identical(
    create_path(filename = "abc.txt", format_stamp = "",
                dir = file.path(my_tempdir, "."), add_date = FALSE),
    normalizePath(file.path(my_tempdir, ".", "abc.txt"), winslash = "/",
                  mustWork = FALSE)
  )
)

# 'directories' that are only working directory followed by a file extension
# are accepted!
expect_silent(
  expect_identical(
    create_path(filename = "abc.txt", format_stamp = "",
                dir = file.path(my_tempdir, ".txt"), add_date = FALSE),
    normalizePath(file.path(my_tempdir, ".txt", "abc.txt"), winslash = "/",
                  mustWork = FALSE)
  )
)

##### Warnings #####
# 'directories' that actually are names of existing files lead to the working
# directory being used instead!
expect_warning(
  expect_identical(
    create_path(filename = "abc.txt", format_stamp = "",
                dir = my_tempfile, add_date = FALSE),
    file.path(getwd(), "abc.txt")
  ), pattern = paste0(basename(my_tempfile), "' already exists"),
  strict = TRUE, fixed = TRUE
)

expect_warning(
  expect_identical(
    create_path(filename = "abc.txt", format_stamp = "",
                dir = my_tempfile, add_date = FALSE),
    file.path(getwd(), "abc.txt")
  ), pattern = "Attempt to create directory", strict = TRUE, fixed = TRUE
)

# A warning is issued if the file indicated by the returned path already exists.
expect_warning(
  expect_identical(
    create_path(filename = basename(my_tempfile), format_stamp = "",
                dir = my_tempdir, add_date = FALSE),
    my_tempfile),
  pattern = "File already exists:", strict = TRUE, fixed = TRUE)

filenm_in <- c("c#c.txt", "d d.txt")
filenm_out <- c("c.c.txt", "d.d.txt")
for(ind_filenm in seq_along(filenm_in)) {
  expect_warning(
    expect_identical(
      create_path(filename = filenm_in[ind_filenm], format_stamp = "",
                  dir = my_tempdir, add_date = FALSE),
      normalizePath(file.path(my_tempdir, filenm_out[ind_filenm]),
                    winslash = "/", mustWork = FALSE)
    ),
    pattern = paste0("Replaced non-alphanumeric characters other than",
                     " underscores in filename\n'", filenm_in[ind_filenm],
                     "' with dots: ", filenm_out[ind_filenm]),
    strict = TRUE, fixed = TRUE)
}

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
               pattern = paste0("'filename' should include the name and the",
                                " file extension: ", filenm_in),
               fixed = FALSE)
}

filenm_in <- c("a/a.txt", "b\\b.txt")
for(ind_filenm in seq_along(filenm_in)) {
  expect_error(
    create_path(filename = filenm_in[ind_filenm], format_stamp = "",
                dir = my_tempdir, add_date = FALSE),
    pattern = paste0("Filename '",
                     filenm_in[ind_filenm], "' contains (back)slashes"),
    fixed = TRUE)
}

##### Check 'format_stamp' #####
expect_error(
  create_path(filename = "abc.txt", format_stamp = character(0),
              dir = my_tempdir, add_date = TRUE),
  pattern = "checkinput::is_character(format_stamp, allow_empty = TRUE) is not TRUE", fixed = TRUE)

expect_error(
  create_path(filename = "abc.txt", format_stamp = NA_character_,
              dir = my_tempdir, add_date = TRUE),
  pattern = "checkinput::is_character(format_stamp, allow_empty = TRUE) is not TRUE", fixed = TRUE)

expect_error(
  create_path(filename = "abc.txt", format_stamp = c("", "%d_%m_%Y"),
              dir = my_tempdir, add_date = TRUE),
  pattern = "checkinput::is_character(format_stamp, allow_empty = TRUE) is not TRUE", fixed = TRUE)

##### Check 'dir' #####
for(dir in list(3, "", character(0), NULL, c("temp_p1", "temp_p2"))) {
  expect_error(
    create_path(filename = "abc.txt", dir = dir),
    pattern = "checkinput::is_character(dir) is not TRUE", fixed = TRUE)
}

for(dir in list(paste0(my_tempdir, "./"), paste0(my_tempdir, "temp_p1/"))) {
  expect_error(
    create_path(filename = "abc.txt", dir = dir),
    pattern = "'dir' should not end with '/'", fixed = TRUE)
}

for(dir in list(paste0(my_tempdir, ".\\"), paste0(my_tempdir, "temp_p1\\"))) {
  expect_error(
    create_path(filename = "abc.txt", dir = dir),
    pattern = "'dir' should not end with '\\'", fixed = TRUE)
}

for(dir in list(paste0(my_tempdir, ".."), paste0(my_tempdir, "temp_p1."))) {
  expect_error(
    create_path(filename = "abc.txt", dir = dir),
    pattern = "'dir' should not end with '.'", fixed = TRUE)
}

for(dir in list(paste0(my_tempdir, ". "), paste0(my_tempdir, "temp_p1 "))) {
  expect_error(
    create_path(filename = "abc.txt", dir = dir),
    pattern = "'dir' should not end with ' ' (i.e., a space)", fixed = TRUE)
}

##### Check 'add_date' #####
for(add_date in list(3, NA)) {
  expect_error(
    create_path(filename = "abc.txt", dir = my_tempdir, add_date = add_date),
    pattern = "checkinput::is_logical(add_date) is not TRUE", fixed = TRUE)
}


#### Cleaning up ####
##### Delete the created temporary files #####
unlink(
  x = file.path(my_tempdir,
                c(".txt",
                  format(Sys.time(), format = "%Y_%m_%d"),
                  "subdir",
                  "test_df.csv")),
  recursive = TRUE)


##### Remove objects used in tests #####
rm(add_date, dir, filenm, filenm_in, filenm_out, ind_filenm, my_tempdir,
   my_tempfile)
