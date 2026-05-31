#### Create objects to use in tests ####
Windows_reserved <- c("CON", "PRN", "AUX", "NUL", paste0("COM", 1:9),
                      paste0("LPT", 1:9))
Windows_reserved <- c(Windows_reserved, tolower(Windows_reserved))
illegal_chars <- c('"', "*", "?", "|", "<", ">")
warn_Windows_reserved <- "should not contain Windows-reserved names"
warn_space_dot <- "should not end with ' ' or '.'"


#### Test the examples ####
expect_true(is_path(getwd()))
expect_true(is_path(fs::path_wd("abcd")))
expect_error(is_path(fs::path_wd("ab|cd")),
             pattern = "should not contain '\"', '*'", fixed = TRUE)

expect_true(is_path(fs::path_wd("abcd.txt")))
expect_true(is_path(fs::path_wd("abcd.txt.gz")))
expect_true(is_path(fs::path_wd("abcd.gz")))

expect_error(is_path(fs::path_wd("ab:cd.txt")),
             pattern = "'filename' should not contain ':'", fixed = TRUE)
expect_error(is_path(fs::path_wd("ab|cd.txt")),
             pattern = "should not contain '\"', '*'", fixed = TRUE)


#### Tests ####
##### Illegal characters #####
for(illegal_char in illegal_chars) {
  expect_error(is_path(path = fs::path_wd(paste0("ab", illegal_char, "cd"))),
               pattern = "should not contain '\"', '*'", fixed = TRUE)
}

for(illegal_char in illegal_chars) {
  expect_error(is_path(path = fs::path_wd(paste0("ab", illegal_char, "cd.txt"))),
               pattern = "should not contain '\"', '*'", fixed = TRUE)
}

expect_error(is_path(path = fs::path_wd("ab:cd.txt")),
             pattern = "'filename' should not contain ':'", fixed = TRUE)

for(control_char in paste0("\005", "\025", "\035", "\177")) {
  expect_error(is_path(path = fs::path_wd(paste0("ab", control_char, "cd"))),
               pattern = "should not contain control characters", fixed = TRUE)
  expect_error(is_path(path = fs::path_wd(paste0("ab", control_char, "cd.txt"))),
               pattern = "should not contain control characters", fixed = TRUE)
}

##### Windows reserved names #####
# These are not allowed as path components but are allowed as filename
for(Windows_name in Windows_reserved) {
  expect_error(is_path(path = fs::path_wd(Windows_name)),
               pattern = warn_Windows_reserved, fixed = TRUE)
  expect_error(is_path(path = fs::path_wd("subdir", Windows_name, "filename.txt")),
               pattern = warn_Windows_reserved, fixed = TRUE)
  expect_true(is_path(path = fs::path_wd("subdir", paste0(Windows_name, ".txt"),
                                         "filename.txt")))
  expect_true(is_path(path = fs::path_wd("subdir", paste0(Windows_name, ".txt"))))
}

# 'COM', 'COM0', 'LPT' and 'LPT0' are allowed as filename and as path component
for(Windows_allowed in c("COM", "COM0", "LPT", "LPT0")) {
  expect_true(is_path(path = fs::path_wd(Windows_allowed)))
  expect_true(is_path(path = fs::path_wd("subdir", Windows_allowed, "filename.txt")))
  expect_true(is_path(path = fs::path_wd("subdir", paste0(Windows_allowed, ".txt"),
                                         "filename.txt")))
  expect_true(is_path(path = fs::path_wd("subdir", paste0(Windows_allowed, ".txt"))))
}

##### Spaces and dots #####
# Path elements should not end with a space
expect_true(is_path(fs::path("a b", "def")))
expect_true(is_path(fs::path("a  b", "def")))
expect_error(is_path(path = fs::path("ab", " ", "def")),
             pattern = warn_space_dot, fixed = TRUE)
expect_error(is_path(path = fs::path("ab", "  ", "def")),
             pattern = warn_space_dot, fixed = TRUE)
expect_error(is_path(path = fs::path("ab ", "def")),
             pattern = warn_space_dot, fixed = TRUE)
expect_error(is_path(path = fs::path("ab  ", "def")),
             pattern = warn_space_dot, fixed = TRUE)
expect_error(is_path(path = fs::path("ab", "def ")),
             pattern = warn_space_dot, fixed = TRUE)
expect_error(is_path(path = fs::path("ab", "def ")),
             pattern = warn_space_dot, fixed = TRUE)

# "." and ".." are only allowed as first path component
expect_true(is_path(fs::path(".", "a.b", "def")))
expect_true(is_path(fs::path("..", "a..b", "def")))
expect_error(is_path(path = fs::path("ab", "..", "def")),
             pattern = warn_space_dot, fixed = TRUE)
expect_error(is_path(path = fs::path("ab", ".", "def")),
             pattern = warn_space_dot, fixed = TRUE)

# Path elements should not end with a dot
expect_error(is_path(path = fs::path("ab.", "def")),
             pattern = warn_space_dot, fixed = TRUE)
expect_error(is_path(path = fs::path("ab..", "def")),
             pattern = warn_space_dot, fixed = TRUE)
expect_error(is_path(path = fs::path("ab", "def.")),
             pattern = warn_space_dot, fixed = TRUE)
expect_error(is_path(path = fs::path("ab", "def..")),
             pattern = warn_space_dot, fixed = TRUE)

# Filenames should not end with a space or a dot
# expect_error(is_path(path = fs::path_wd("subdir", "filename ")),
#              pattern = warn_space_dot, fixed = TRUE)
# expect_error(is_path(path = fs::path_wd("subdir", "filename .txt")),
#              pattern = warn_space_dot, fixed = TRUE)
# expect_error(is_path(path = fs::path_wd("subdir", "filename.")),
#              pattern = warn_space_dot, fixed = TRUE)
# expect_error(is_path(path = fs::path_wd("subdir", "filename..txt")),
#              pattern = warn_space_dot, fixed = TRUE)

# Filenames should not end with a space or a dot
expect_equal(is_path(path = fs::path_wd("subdir", "filename..txt"),
                         test_is_path = TRUE),
             list(c("function (path) , {,     dir <- path_dir(path),     file <- sub(\"(?<!^|[.])\\\\.+([^.]+)$\", \"\", path_file(path), ,         perl = TRUE),     na <- is.na(path),     no_dir <- dir == \".\" | dir == \"\",     path[!na & no_dir] <- path_tidy(file[!na & no_dir]),     path[!na & !no_dir] <- path(dir[!na & !no_dir], file[!na & ,         !no_dir]),     path, }"),
                  c("function (path) , {,     if (length(path) == 0) {,         return(character()),     },     res <- captures(path_file(path), regexpr(\"(?<!^|[.]|/)[.]+([^.]+)$\", ,         path_file(path), perl = TRUE))[[1]],     res[!is.na(path) & is.na(res)] <- \"\",     res, }")))


##### Temporary directory #####
expect_error(is_path(path = tempdir()),
             pattern = "'path' should not point to 'tempdir()'", fixed = TRUE)
expect_true(is_path(fs::path(tempdir(), "subdir")))


##### Repeated file separators #####
# Need file.path() because fs::path_wd() removes repeated file separators
expect_warning(
  expect_true(is_path(path = file.path(fs::path_wd("subdir"), "/filename.txt"))),
  pattern = "Repeated '/' or '\\\\' in", fixed = TRUE, strict = TRUE)

expect_warning(
  expect_true(is_path(path = file.path(fs::path_wd("subdir"), "\\filename.txt"))),
  pattern = "Repeated '/' or '\\\\' in", fixed = TRUE, strict = TRUE)

expect_warning(
  expect_true(is_path(path = file.path(fs::path_wd("subdir"), "\\\\filename.txt"))),
  pattern = "Repeated '/' or '\\\\' in", fixed = TRUE, strict = TRUE)

##### Trailing file separators #####
# To prevent warning about repeated file separators on MacOS and Ubuntu (where
# the paths will end in a slash)
path_in <- fs::path_wd("subdir", "filename.txt")
if(endsWith(path_in, suffix = "/")) {
  expect_true(is_path(path = path_in))
} else {
  expect_true(is_path(path = paste0(path_in, "/")))
}

expect_true(is_path(path = paste0(fs::path_wd("subdir", "filename.txt"), "\\")))


#### Cleaning up ####
rm(control_char, illegal_char, illegal_chars, warn_space_dot,
   warn_Windows_reserved, Windows_allowed, Windows_name, Windows_reserved)
