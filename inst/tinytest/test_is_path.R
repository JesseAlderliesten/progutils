warning("Incomplete testing in test_is_path.R!")


#### Create objects to use in tests ####
warn_Windows_reserved <- "should not contain Windows-reserved names"


#### Tests ####
for(Windows_name in c("CON", "PRN", "AUX", "NUL", paste0("COM", 1:9),
                      paste0("LPT", 1:9))) {
  expect_error(is_path(path = file.path(getwd(), Windows_name)),
               pattern = warn_Windows_reserved, fixed = TRUE)
  expect_error(is_path(path = file.path(getwd(), Windows_name, "filename.txt")),
               pattern = warn_Windows_reserved, fixed = TRUE)
}

# 'COM', 'COM0', 'LPT' and 'LPT0' are allowed
for(filename in c("COM", "COM0", "LPT", "LPT0")) {
  expect_true(is_path(path = paste0(filename, ".txt")))
}

expect_true(is_path(file.path(".", "a.b", "def")))
expect_error(is_path(path = file.path("ab", ".", "def")),
             pattern = "should not end with ' ' or '.'", fixed = TRUE)
expect_error(is_path(path = file.path("ab.", "def")),
             pattern = "should not end with ' ' or '.'", fixed = TRUE)
