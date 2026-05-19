#### Create objects to use in tests ####
warn_invalid <- "Empty filename or missing extension"
warn_slash <- "'filename' should not contain '/' or '\\'"


#### Tests ####
expect_true(is_filename("a.txt"))
expect_true(is_filename("a.txt.gz"))

# These are not allowed as path components but are allowed as filenames
illegal_paths <- c("CON", "PRN", "AUX", "NUL", paste0("COM", 1:9),
                   paste0("LPT", 1:9))
for(Windows_name in paste0(illegal_paths, ".txt")) {
  expect_true(is_filename(filename = Windows_name))
}

# 'COM', 'COM0', 'LPT' and 'LPT0' are even allowed as path components
for(filename in c("COM", "COM0", "LPT", "LPT0")) {
  expect_true(is_filename(paste0(filename, ".txt")))
}

##### Dots and spaces #####
expect_true(is_filename("abcd.txt"))
expect_true(is_filename(".abcd.txt"))
expect_true(is_filename("ab.cd.txt"))
expect_error(is_filename("abcd..txt"),
             pattern = "should not end with '.'", fixed = TRUE)
expect_error(is_filename("..txt"), pattern = warn_invalid, fixed = TRUE)
expect_error(is_filename("...txt"), pattern = warn_invalid, fixed = TRUE)

expect_error(is_filename(" abcd.txt"),
             pattern = "should not start with ' ' (i.e., a space)", fixed = TRUE)
expect_error(is_filename("abcd .txt"),
             pattern = "should not end with ' ' (i.e., a space)", fixed = TRUE)

##### Containing directories #####
expect_error(is_filename("somedir/a.txt"), pattern = warn_slash, fixed = TRUE)
expect_error(is_filename("./a.txt"), pattern = warn_slash, fixed = TRUE)
expect_error(is_filename("../a.txt"), pattern = warn_slash, fixed = TRUE)
expect_error(is_filename("somedir\\a.txt"), pattern = warn_slash, fixed = TRUE)
expect_error(is_filename(".\\a.txt"), pattern = warn_slash, fixed = TRUE)
expect_error(is_filename("..\\a.txt"), pattern = warn_slash, fixed = TRUE)
expect_error(is_filename("ab/cd.txt"), pattern = warn_slash, fixed = TRUE)
expect_error(is_filename("ab\\cd.txt"), pattern = warn_slash, fixed = TRUE)

##### Compression extensions #####
expect_true(is_filename("abcd.txt.gz"))
expect_true(is_filename("abcd.txt.bz2"))
expect_true(is_filename("abcd.txt.xz"))

##### Missing extension #####
expect_error(is_filename("abcd"), pattern = warn_invalid, fixed = TRUE)
expect_error(is_filename("abcd.gz"), pattern = warn_invalid, fixed = TRUE)
expect_error(is_filename("abc.tx#"), pattern = warn_invalid, fixed = TRUE)

##### Empty filenames #####
expect_error(is_filename(".txt"), pattern = warn_invalid, fixed = TRUE)
expect_error(is_filename(".txt.gz"), pattern = warn_invalid, fixed = TRUE)
expect_error(is_filename(".gz"), pattern = warn_invalid, fixed = TRUE)

##### Illegal characters #####
for(illegal_char in c('"', "*", ":", "?", "|", "<", ">")) {
  expect_error(is_filename(paste0("ab", illegal_char, "cd.txt")),
               pattern = "should not contain any of", fixed = TRUE)
}

##### Invalid input #####
expect_error(is_filename(1),
             pattern = "checkinput::is_character(filename) is not TRUE",
             fixed = TRUE)

expect_error(is_filename(c("abcd", "efgh")),
             pattern = "checkinput::is_character(filename) is not TRUE",
             fixed = TRUE)


#### Remove objects used in tests ####
rm(filename, illegal_char, warn_invalid, warn_slash, Windows_name)
