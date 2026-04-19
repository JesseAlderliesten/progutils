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

# Prior to R 4.6.0, this produced the nonsense-result "ab..txt.txt" instead of
# re-creating 'filename' because tools::file_path_sans_ext() did not remove the
# extension if the filename ended in a dot.
if(getRversion() < "4.6.0") {
  expect_identical(
    paste0(tools::file_path_sans_ext(filename), ".", tools::file_ext(filename)),
    "ab..txt.txt")
} else {
  expect_identical(
    paste0(tools::file_path_sans_ext(filename), ".", tools::file_ext(filename)),
    filename)
}

# Correct result
expect_silent(
  expect_identical(
    paste0(progutils::file_path_sans_ext(filename), ".", tools::file_ext(filename)),
    filename)
)


#### Remove objects used in tests ####
rm(filename)
