#### Create objects to use in tests ####
filename <- "ab..txt"


#### Test the examples ####
expect_silent(
  expect_identical(
    tools::file_path_sans_ext(filename),
    "ab..txt")
)

expect_silent(
  expect_identical(
    tools::file_ext(filename),
    "txt")
)

# Nonsense-result: the correct result would give 'filename', i.e., "ab..txt"
expect_silent(
  expect_identical(
    paste0(tools::file_path_sans_ext(filename), ".", tools::file_ext(filename)),
    "ab..txt.txt")
)

# Correct result
expect_silent(
  expect_identical(
    paste0(progutils::file_path_sans_ext(filename), ".", tools::file_ext(filename)),
    filename)
)


#### Remove objects used in tests ####
rm(filename)
